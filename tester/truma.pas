unit truma;

{$mode ObjFPC}{$H+}

{ definitions and utilities to test the functionality of a lin master
  to control a combi d
}

interface

uses
  Classes, SysUtils,lin,trumautils,trumaframes;

const FRAMEDELAY=0;

type
  { TTrumaD }

  TMasterReplyNotification = procedure(const index:integer; const payload:string) of object;
  TOnFrameReceived = procedure(const id:byte) of object;
  TMessageNotification = procedure(const msg:string) of object;
  TTrumaD = class (TThread)
    private
      FOpened:boolean;
      FLinMaster:TLinMaster;
      FSetPointTemp:extended;
      FSimulTemp:extended;
      FBoilerMode:TBoilerMode;
      FFanMode:TFanMode;
      FFanSpeed:integer;
      FOnOff:boolean;
      FOldFrame:array[1..255] of string;
      FDiag:array[1..2] of string;
      FDiagCycle:integer;
      FFrameId:byte;
      FOnFrameReceived:TOnFrameReceived;
      FOnDiag:TMasterReplyNotification;
      FOnMessage:TMessageNotification;
      FMessage:string;
      Fwaterboost : record
        active:boolean;
        finished:boolean;
        remaining:integer;
        start:qword;
        oldWaterRequest:boolean;
      end;
      FWaterDemand:boolean;
      FWaterTemperature:extended;
      FResetError:boolean;
      FFrame16:TFrame16;
      FFrame14:TFrame14;
      FFrame34:TFrame34;
      FFrame37:TFrame37;
      FFrame39:TFrame39;
      FFrame35:TFrame35;
      FFrame3b:TFrame3b;
      procedure NotifyFrame;
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
      constructor create(const port:String; OnFramereceived:TOnFrameReceived; OnDiag:TMasterReplyNotification; OnMessage:TMessageNotification);
      destructor Destroy;override;
      property Opened:boolean read FOpened;
      property SetPointTemp:extended read FSetPointTemp write SetSetpointTemp;
      property SimulTemp:extended read FSimulTemp write FSimulTemp;
      property BoilerMode:TBoilerMode read FBoilerMode write SetBoilerMode;
      property FanMode:TFanMode read FFanMode write SetFanMode;
      property FanSpeed:integer read FFanSpeed write SetFanSpeed;
      property FakeReceive:boolean write SetFakeReceive;
      property OnOff:boolean read FOnOff write FOnOff;
      procedure ResetError;
      property Frame16:TFrame16 read FFrame16;
      property Frame14:TFrame14 read FFrame14;
      property Frame34:TFrame34 read FFrame34;
      property Frame37:TFrame37 read FFrame37;
      property Frame39:TFrame39 read FFrame39;
      property Frame35:TFrame35 read FFrame35;
      property Frame3b:TFrame3b read FFrame3b;
  end;

implementation


{ TTrumaD }

procedure TTrumaD.NotifyFrame;
begin
  if assigned(FOnFrameReceived) then
    FOnFrameReceived(FFrameId);
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
  PumpOrFan: byte;
  LocSetPointTemp: Extended;
  TimeElapsed: QWord;
  MinutesRemaining, i: integer;

  procedure FrameSleep;
  begin
    if FRAMEDELAY>0 then
      Sleep(FRAMEDELAY)
  end;

  function WriteFrame(const id:byte; const data:string):boolean;
  begin
    result:=FLinMaster.WriteFrame(id, data);
    if not result then
       DoMessage(format('error write %.2x: %s',[id,FLinMaster.LastErrorDesc]));
  end;
  function ReadFrame(const id:UInt8; var dest; const expectedlen:Uint8):boolean;
  var LocData:string;
  begin
    FFrameId:=id;
    result:=FLinMaster.ReadFrame(id, LocData, expectedlen);
    if result then
    begin
      if (id=$3c) or (id=$3d) or (LocData<>FOldFrame[id]) then
      begin
        Move(LocData[1],dest,8);
        if (id<>$3c) and (id<>$3d) then
          Synchronize(@NotifyFrame);
      end;
    end else
      DoMessage(format('error read %.2x: %s',[id, FLinMaster.LastErrorDesc]))
  end;

  function MasterControlFrame(const sid:byte; const data:string; out reply:string):boolean;
  var LocFrame:String;
    loclen: Byte;
  begin
    SetLength(LocFrame,8);
    FillByte(LocFrame[1],8,$ff);
    loclen:=length(data);
    if loclen>5 then
       loclen:=5;
    Byte(locframe[1]):=$01; //NAD
    Byte(locframe[2]):=loclen+1; //PCI Single frame+len (+1 for the SID)
    Byte(locframe[3]):=sid;
    move(data[1],locframe[4],loclen);
    result:=WriteFrame($3c,LocFrame);
    if not result then
       exit;
    FrameSleep;
    result:=ReadFrame($3d,LocFrame[1],8);
    if not result then
       exit;
    loclen:=byte(locframe[2]);
    if loclen>6 then
    begin
      DoMessage(format('mrf %.2x wrong length %d',[sid,loclen]));
      result:=false;
      exit;
    end;
    if byte(locframe[3])<>sid+64 then
    begin
      DoMessage(format('mrf %.2x wrong rsid, expected %.2x, received %.2x',[sid,sid+64,byte(locframe[3])]));
      result:=false;
      exit;
    end;
    loclen:=loclen-1;
    SetLength(reply,loclen);
    move(locframe[4],reply[1],loclen);
    FrameSleep;
  end;

  function TrumaOnOff(active:boolean):boolean;
  var i:integer;
  begin
    result:=false;
    for i:=1 to 5 do
      if MasterControlFrame($b8,chr($10)+chr($03)+chr(ord(active)),lindata) then
      begin
        result:=true;
        if lindata<>FDiag[1] then
        begin
          FDiagCycle:=1;
          FDiag[1]:=lindata;
          Synchronize(@NotifyDiag);
        end;
        break;
      end;
  end;

  function TrumaGetErrorInfo:boolean;
  begin
    result:=MasterControlFrame($b2,chr($23)+chr($17)+chr($46)+chr($10)+chr($03),lindata);
    if result then
      if lindata<>FDiag[1] then
      begin
        FDiagCycle:=2;
        FDiag[2]:=lindata;
        Synchronize(@NotifyDiag);
      end;
  end;

  function AssignFrameRanges(const StartIndex:byte; pids:array of byte):boolean;
  var locdata:string;
    loclen, p: integer;
  begin
    setlength(locdata,5);
    FillByte(locdata[1],5,$ff);
    byte(locdata[1]):=StartIndex;
    loclen:=length(pids);
    if loclen>4 then
      loclen:=4;
    for p:=0 to high(pids) do
       byte(locdata[2+p]):=TLinMaster.GetProtectedId(pids[p]);
    result:=MasterControlFrame($b7,locdata,lindata);
  end;


begin
  writeln('======================================================');

  if FResetError then
  begin
    FResetError:=false;
    if TrumaOnOff(false) then
      for i:=1 to 200 do
      begin
         if Terminated then
           exit;
         Sleep(50);
      end;
  end;

  TrumaOnOff(FOnOff);

  TrumaGetErrorInfo;

  AssignFrameRanges($09,[$3b,$3a,$39,$38]);
  AssignFrameRanges($0d,[$37,$36,$35,$34]);
  AssignFrameRanges($11,[$33,$32]);

  if ReadFrame($16,FFrame16,8) then //Combi_Complete_V8, Combi_DE_V119
  begin
    FWaterTemperature:=FFrame16.WaterTemperature;
    FWaterDemand:=FFrame16.WaterDemand;
  end;
  FrameSleep;
  if ReadFrame($14, FFRame14, 8) then
    FrameSleep;
  if ReadFrame($34, FFRame34, 8) then
    FrameSleep;
  if ReadFrame($37, FFRame37, 8) then
    FrameSleep;
  if ReadFrame($39, FFRame39, 8) then
    FrameSleep;
  if ReadFrame($35, FFRame35, 8) then
    FrameSleep;
  if ReadFrame($3b, FFRame3b, 8) then
    FrameSleep;

  LocSetPointTemp:=0.0;
  if FSetPointTemp<5.0 then //no heating
  begin
     if FBoilerMode=BoilerOff then
     begin
        if FFanMode=FanOn then
          PumpOrFan:=16+FFanSpeed
        else
          PumpOrFan:=0;
     end else //boiler active
     begin
       PumpOrFan:=0;
     end
  end else //heating active
  begin
    LocSetPointTemp:=FSetPointTemp;
    if FFanMode=FanHigh then
       PumpOrFan:=2
    else
       PumpOrFan:=1;
  end;
  if FBoilerMode=BoilerBoost then
  begin
    if not FWaterBoost.Active then
    begin
      Fwaterboost.start:=GetTickCount64;
      Fwaterboost.oldWaterRequest:=FWaterDemand;
      FWaterboost.active:=true;
      FWaterboost.finished:=false;
      FWaterboost.remaining:=40;
      DoMessage('WaterBoost activated')
    end;
  end else
  begin
    if Fwaterboost.active then
      DoMessage('WaterBoost deactivated');
    Fwaterboost.active:=false;
  end;
  if Fwaterboost.active and not FWaterboost.finished then
  begin
    //with water boost and water at less than 50º stop the heating
    if FWaterTemperature<50.0  then
      LocSetPointTemp:=0;
    //max 40 minutes of water boost
    TimeElapsed:=GetTickCount64-Fwaterboost.Start;
    if TimeElapsed>=40*60*1000 then
    begin
      FWaterBoost.finished:=true;
      DoMessage('WaterBoost active for 40 minutes, stopped');
    end else
    //water request deactivated (temperature reached), turn off boost
    if Fwaterboost.oldWaterRequest and not FWaterDemand then
    begin
      FWaterBoost.finished:=true;
      DoMessage('WaterBoost reached temperature, stopped');
    end else
    begin
       MinutesRemaining:=40-TimeElapsed div 1000 div 60;
       if MinutesRemaining<>Fwaterboost.remaining then
       begin
         Fwaterboost.remaining:=MinutesRemaining;
         DoMessage(format('WaterBoost remaining %d minutes',[MinutesRemaining]))
       end;
    end;
    Fwaterboost.oldWaterRequest:=FWaterDemand;
  end;

  WriteFrame($2,HeaterCommand(FSimulTemp));
  FrameSleep;
  WriteFrame($3,HeaterCommand(LocSetPointTemp));
  FrameSleep;
  WriteFrame($4,BoilerCommand(FBoilerMode));
  FrameSleep;
  WriteFrame($5,EnergySelect(EsGasDiesel));
  FrameSleep;
  WriteFrame($6,SetPowerLimit(EsGasDiesel));
  FrameSleep;
  WriteFrame($7,Fan(PumpOrFan));
  FrameSleep;

  //frames autogenerated from captured data
  //{$include frames.inc}

end;

constructor TTrumaD.create(const port: String; OnFrameReceived:TOnFrameReceived; OnDiag:TMasterReplyNotification; OnMessage:TMessageNotification);
begin
  FDiagCycle:=1;
  FLinMaster:=TLinMaster.Create(port,9600);
  FOpened:=FLinMaster.LastErrorDesc='';
  FLinMaster.Verbose:=true;
  FOnFrameReceived:=OnFrameReceived;
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

procedure TTrumaD.ResetError;
begin
  FResetError:=true;
end;

end.

