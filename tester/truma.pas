unit truma;

{$mode ObjFPC}{$H+}

{ definitions and utilities to test the functionality of a lin master
  to control a combi d
}

interface

uses
  Classes, SysUtils,lin;

const FRAMEDELAY=10;
type
  TBoilerMode = (BoilerOff, BoilerEco, BoilerHot, BoilerBoost);
const
  BoilerTemp:array[TBoilerMode] of integer = (0,40,55,60);
type
  TFanMode = (FanOff, FanEco, FanHigh, FanOn);

  { TTrumaD }

  TNotification = procedure(const index:integer; const payload:string) of object;
  TMessageNotification = procedure(const msg:string) of object;
  TTrumaD = class (TThread)
    private
      FOpened:boolean;
      FLinMaster:TLinMaster;
      FSetPointTemp:extended;
      FBoilerMode:TBoilerMode;
      FFanMode:TFanMode;
      FFanSpeed:integer;
      FFanBoost:boolean;
      FOnOff:boolean;
      FStatus:string;
      FDiag:array[1..2] of string;
      FDiagCycle:integer;
      FOnStatus:TNotification;
      FOnDiag:TNotification;
      FOnMessage:TMessageNotification;
      FMessage:string;
      procedure NotifyStatus;
      procedure NotifyDiag;
      procedure NotifyMessage;
      procedure DoMessage(const msg:string);
      procedure SetBoilerMode(AValue: TBoilerMode);
      procedure SetFakeReceive(AValue: boolean);
      procedure SetFanMode(AValue: TFanMode);
      procedure SetFanSpeed(AValue: integer);
      procedure SetSetpointTemp(AValue: extended);
      procedure Loop;
    protected
      procedure Execute;override;
    public
      constructor create(const port:String; OnStatus,OnDiag:TNotification; OnMessage:TMessageNotification);
      destructor Destroy;override;
      property Opened:boolean read FOpened;
      property SetPointTemp:extended read FSetPointTemp write SetSetpointTemp;
      property BoilerMode:TBoilerMode read FBoilerMode write SetBoilerMode;
      property FanMode:TFanMode read FFanMode write SetFanMode;
      property FanSpeed:integer read FFanSpeed write SetFanSpeed;
      property FakeReceive:boolean write SetFakeReceive;
      property OnOff:boolean read FOnOff write FOnOff;
      property FanBoost:boolean read FFanBoost write FFanBoost;
  end;

type
  TSetPowerLimit = (PowerDiesel, Power900W, Power1800W);
const
  PowerValues : array[TSetPowerLimit] of word = ($0000,$0384,$0708);

type
  TEnergySelect = (EnergyNone, EnergyDiesel, EnergyElectricity, EnergyMix);


  function HeaterCommand(temperature:extended):string;
  function BoilerCommand(mode:TBoilerMode):string;
  function EnergySelect(const mode:TEnergySelect):string;
  function SetPowerLimit(const limit:TSetPowerLimit):string;
  function Fan(mode:TFanMode; speed:integer; RoomBoost:boolean):string;
  function DiagFrame(diagCycle:integer;onoff:boolean):string;

  function DecodeTempKelvin(const b1,b2:byte):Extended;
  function DecodeVoltage(const b1,b2:byte):Extended;
  procedure EncodeTempKelvin(const t:extended; out b1,b2:byte);
  //function DecodePowerLimit(const b1,b2:byte):word;
  procedure EncodePowerLimit(const p:TSetPowerLimit; out b1,b2:byte);


implementation


function HeaterCommand(temperature: extended): string;
begin
  setlength(result,8);
  FillByte(result[1],8,$FF);
  EncodeTempKelvin(temperature,byte(result[1]),byte(result[2]));
end;

function BoilerCommand(mode: TBoilerMode): string;
begin
  setlength(result,8);
  FillByte(result[1],8,$FF);
  EncodeTempKelvin(BoilerTemp[mode],byte(result[1]),byte(result[2]));
end;

function EnergySelect(const mode:TEnergySelect): String;
begin
  setlength(result,8);
  FillByte(result[1],8,$FF);
  byte(result[1]):=ord(mode);
end;

function SetPowerLimit(const limit:TSetPowerLimit): String;
begin
  setlength(result,8);
  FillByte(result[1],8,$FF);
  EncodePowerLimit(limit,byte(result[1]),byte(result[2]));
end;



function Fan(mode:TFanMode; speed:integer; RoomBoost:boolean):String;
begin
  setlength(result,8);
  FillByte(result[1],8,$FF);
  if mode=FanOn then
     byte(result[1]):=$10 or (speed and $f)
  else
     byte(result[1]):=ord(mode);
  if RoomBoost then
    byte(result[2]):=$01
  else
    byte(result[2]):=$00
end;

function DiagFrame(diagCycle:integer; onoff: boolean): String;
const DiagFrames:array[1..2] of array[0..7] of byte = (
  ($01, $04, $B8, $10, $03, $00, $FF, $FF),     //el ultimo byte antes del FF FF debería se 01 con alguna función activa?
  ($01, $06, $B2, $23, $17, $46, $10, $03)
  );

begin
  writeln('----- diagCycle ',diagCycle);
  SetLength(result,8);
  move(DiagFrames[diagCycle][0],result[1],8);
  if onoff and (diagCycle=1) then
    byte(result[6]):=$01;
end;

function DecodeTempKelvin(const b1, b2: byte): Extended;
begin
  result:=(b1+b2*256)*0.1-273;
end;

function DecodeVoltage(const b1, b2: byte): Extended;
begin
  result:=(b1+b2*256)*0.01-327.67;
end;

procedure EncodeTempKelvin(const t: extended; out b1, b2: byte);
var w:word;
begin
  w:=trunc((t+273)*10);
  b1:=lo(w);
  b2:=hi(w);
end;

procedure EncodePowerLimit(const p: TSetPowerLimit; out b1, b2: byte);
begin
  b1:=lo(PowerValues[p]);
  b2:=hi(PowerValues[p])
end;

{ TTrumaD }

procedure TTrumaD.NotifyStatus;
begin
  if assigned(FOnStatus) then
    FOnStatus(0,FStatus);
end;

procedure TTrumaD.NotifyDiag;
begin
  if Assigned(FOnDiag) then
    FOnDiag(FDiagCycle,FDiag[FDiagCycle]);
end;

procedure TTrumaD.NotifyMessage;
begin
  if Assigned(FOnMessage) then
     FOnMessage(FMessage)
end;

procedure TTrumaD.DoMessage(const msg: string);
begin
  FMEssage:=msg;
  Synchronize(@NotifyMessage);
end;

procedure TTrumaD.SetBoilerMode(AValue: TBoilerMode);
begin
  if FBoilerMode=AValue then Exit;
  FBoilerMode:=AValue;
end;

procedure TTrumaD.SetFakeReceive(AValue: boolean);
begin
  FLinMaster.FakeReceive:=AValue;
end;

procedure TTrumaD.SetFanMode(AValue: TFanMode);
begin
  if FFanMode=AValue then Exit;
  FFanMode:=AValue;
end;

procedure TTrumaD.SetFanSpeed(AValue: integer);
begin
  if FFanSpeed=AValue then Exit;
  FFanSpeed:=AValue;
end;

procedure TTrumaD.SetSetpointTemp(AValue: extended);
begin
  if FSetPointTemp=AValue then Exit;
  FSetPointTemp:=AValue;
end;

procedure TTrumaD.Execute;
begin
  if not FOpened then
    exit;
  while not Terminated do
    Loop;
end;

procedure TTrumaD.Loop;
var
  lindata: string;
  function WriteFrame(const id:byte; const data:string):boolean;
  begin
    result:=FLinMaster.WriteFrame(id, data);
    if not result then
       DoMessage(format('error write %.2x: %s',[id,FLinMaster.LastErrorDesc]));
  end;
  function ReadFrame(const id:UInt8; out data:string; const expectedlen:Uint8):boolean;
  begin
    result:=FLinMaster.ReadFrame(id, data, expectedlen);
    if not result then
       DoMessage(format('error read %.2x: %s',[id, FLinMaster.LastErrorDesc]))
  end;
begin
  writeln('======================================================');

  WriteFrame($3,HeaterCommand(FSetPointTemp));
  Sleep(FRAMEDELAY);
  WriteFrame($4,BoilerCommand(FBoilerMode));
  Sleep(FRAMEDELAY);
  WriteFrame($5,EnergySelect(EnergyDiesel));
  Sleep(FRAMEDELAY);
  WriteFrame($6,SetPowerLimit(PowerDiesel));
  Sleep(FRAMEDELAY);
  //if FSetPointTemp>0 then
  //begin
  //   WriteFrame($7,Fan(FanEco,0,false))
  //end else
     WriteFrame($7,Fan(FFanMode,FFanSpeed,FFanBoost));
  Sleep(FRAMEDELAY);
  {  truma doesn't reply to these frames
  ReadFrame($3a,lindata,8); //Combi_Complete_V8, Combi_DE_V119
  Sleep(FRAMEDELAY);

  ReadFrame($39,lindata,8); //Combi_Complete_V8, Combi_DE_V119
  Sleep(FRAMEDELAY);

  ReadFrame($38,lindata,8); //Combi_Complete_V8, Combi_DE_V119
  Sleep(FRAMEDELAY);

  ReadFrame($32,lindata,8); //Combi_Complete_V8
  Sleep(FRAMEDELAY);

  ReadFrame($37,lindata,8); //Combi_Complete_V8, Combi_DE_V119
  Sleep(FRAMEDELAY);

  ReadFrame($36,lindata,8); //Combi_Complete_V8, Combi_DE_V119
  Sleep(FRAMEDELAY);

  ReadFrame($35,lindata,8); //Combi_Complete_V8, Combi_DE_V119
  Sleep(FRAMEDELAY);

  ReadFrame($34,lindata,8); //Combi_Complete_V8, Combi_DE_V119
  Sleep(FRAMEDELAY);

  ReadFrame($33,lindata,8); //Combi_Complete_V8, Combi_DE_V119
  Sleep(FRAMEDELAY);
  }

  { useless data, unless proven otherwise
  ReadFrame($14,lindata,8); //Combi_Complete_V8, Combi_DE_V119
  Sleep(FRAMEDELAY);

  ReadFrame($15,lindata,4); //Combi_Complete_V8
  Sleep(FRAMEDELAY);
  }

  if ReadFrame($16,lindata,8) then //Combi_Complete_V8, Combi_DE_V119
  begin
    if lindata<>FStatus then
    begin
      FStatus:=lindata;
      Synchronize(@NotifyStatus);
    end else
      Sleep(FRAMEDELAY);
  end;

  {
  ReadFrame($1a,lindata,7); //Yellow led status?
  Sleep(FRAMEDELAY);
  }
  {
  ReadFrame($3b,lindata,8); //Combi_DE_V119
  Sleep(FRAMEDELAY);
  }
  if WriteFrame($3c,DiagFrame(FDiagCycle, FOnOff {(FFanMode<>FanOff) or (FSetPointTemp>0.0) or (FBoilerMode<>BoilerOff)})) then
  begin
    Sleep(FRAMEDELAY);
    if ReadFrame($3d,lindata,8) then
    begin
      if lindata<>FDiag[FDiagCycle] then
      begin
        FDiag[FDiagCycle]:=lindata;
        Synchronize(@NotifyDiag)
      end else
        Sleep(FRAMEDELAY);
    end;
  end;
  FDiagCycle:=3-FDiagCycle;

  //frames autogenerated from captured data
  //{$include frames.inc}

end;

constructor TTrumaD.create(const port: String; OnStatus,OnDiag:TNotification; OnMessage:TMessageNotification);
begin
  FDiagCycle:=1;
  FLinMaster:=TLinMaster.Create(port,9600);
  FOpened:=FLinMaster.LastErrorDesc='';
  FLinMaster.Verbose:=true;
  FOnStatus:=OnStatus;
  FOnMessage:=OnMessage;
  FOnDiag:=OnDiag;
  if not FOpened then
    Writeln(FLinMaster.LastErrorDesc);
  inherited Create(false);
end;

destructor TTrumaD.Destroy;
begin
  inherited Destroy;
end;

end.

