unit receiver;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,trumautils,Serial;

type

TOnSetpoint = procedure (const water:boolean; setpoint:extended) of object;
TOnFan = procedure (const fan:byte) of object;
TOnOnoff = procedure (const onoff:boolean) of object;
TOnMessage = procedure (const msg:string) of object;
TDiagReply2 = array[1..3] of byte;
{ TTrumaReceiver }


 TTrumaReceiver = class(TThread)
  private
    FPort:TSerialHandle;
    FTemperature:extended;
    FTemperatureWater:extended;
    FVoltage:extended;
    FDiagReply1:byte;
    FDiagReply2:TDiagReply2;
    FStatusByte:byte;
    FSetPointReceived:array[boolean] of Extended;
    FSetPointWater:boolean;
    FFan:byte;
    FOnoff:boolean;
    FMessage:string;

    FOnSetpoint:TOnSetpoint;
    FOnFan:TOnFan;
    FOnOnoff:TOnOnoff;
    FOnMessage:TOnMessage;
    function GetOpened: boolean;
    procedure Log(const m: string);
    procedure NotifySetpoint;
    procedure NotifyFan;
    procedure NotifyOnOff;
    procedure Synclog;
  protected
    Procedure Execute;override;
  public
    Constructor Create(ComPort:string; AOnSetpoint:TOnSetpoint; AOnFan:TOnFan; AOnOnoff: TOnOnoff; AOnMessage:TOnMessage);
    property Opened:boolean read GetOpened;
    //don't really care for atomicity
    property StatusByte:byte read FStatusByte write FStatusByte;
    property Temperature:extended read FTemperature write FTemperature;
    property TemperatureWater:extended read FTemperatureWater write FTemperatureWater;
    property Voltage:extended read FVoltage write FVoltage;
    property DiagReply1:byte read FDiagReply1 write FDiagReply1;
    property DiagReply2:TDiagReply2 read FDiagReply2 write FDiagReply2;
end;

implementation

{ TTrumaReceiver }

function TTrumaReceiver.GetOpened: boolean;
begin
  result:=FPort>0;
end;

procedure TTrumaReceiver.Log(const m:string);
begin
  FMessage:=m;
  Synchronize(@SyncLog);
end;

procedure TTrumaReceiver.NotifySetpoint;
begin
  if Assigned(FOnSetpoint) then
    FOnSetpoint(FSetPointWater, FSetPointReceived[FSetpointWater])
end;

procedure TTrumaReceiver.NotifyFan;
begin
  if Assigned(FOnFan) then
    FOnFan(FFan);
end;

procedure TTrumaReceiver.NotifyOnOff;
begin
  if Assigned(FOnOnoff) then
      FOnOnoff(FOnoff);
end;

procedure TTrumaReceiver.Synclog;
begin
  if Assigned(FOnMessage) then
      FOnMessage(FMessage);
end;

procedure TTrumaReceiver.Execute;
var c, pid, id, NewFan:byte;
  status:(WaitBreak,WaitSync,WaitPid);
  lindata:string;
  w:word;
  i:integer;
  mrsid,nad:byte;
  checksumreceived, framereceived, NewOnOff:boolean;
  chksum, expected: UInt8;
  NewSetPointReceived: Extended;
  oldtick: QWord;

  procedure SendReply;
  var
    i: Integer;
    dummy:byte;
  begin
     FSetPointReceived[false]:=-1000;
     FSetPointReceived[true]:=-1000;

     chksum:=GetChecksum(pid,lindata);
     SerWrite(FPort,lindata[1],8);
     SerWrite(FPort,chksum,1);
     SerDrain(FPort);
     //read back sent data
     for i:=1 to 9 do
       if SerReadTimeout(FPort,dummy,1,20)<>1 then
       begin
         Log('readback timeout');
         break
       end else
       begin
         if (i<9) and (dummy<>byte(lindata[i])) then
           Log('===================== mismatch '+inttohex(byte(lindata[i]),2)+' '+inttohex(dummy,2))
       end;
  end;

begin
  if not Opened then
    exit;
  status:=WaitBreak;
  mrsid:=0;
  setlength(lindata,8);
  oldtick:=GetTickCount64;
  while not Terminated do
  begin
    if SerReadTimeout(FPort,c,1,100)<>1 then
    begin
      Status:=WaitBreak;
      if GetTickCount64-oldtick>=1000 then
      begin
        Log('not receiving');
        oldtick:=GetTickCount64;
      end;
      continue;
    end;
   oldtick:=GetTickCount64;
   case Status of
    WaitBreak:
      begin
        if c=0 then
        begin
          Status:=WaitSync;
        end else
        begin
          Log('waiting break, discarding '+inttohex(c,2));
        end;
      end;
    WaitSync:
      begin
        if c=$55 then
        begin
          Status:=WaitPid;
        end else
        begin
          Log('waiting sync, discarding '+inttohex(c,2));
          Status:=WaitBreak;
        end;
      end;
    WaitPid:
      begin
        pid:=c;
        id:=pid and $3f;
        case id of
           $16:
             begin
               byte(lindata[1]):=0;
               byte(lindata[2]):=FStatusByte;
               EncodeTempKelvin(FTemperature, byte(lindata[3]),byte(lindata[4]));
               EncodeTempKelvin(FTemperatureWater, byte(lindata[5]),byte(lindata[6]));
               w:=trunc(Fvoltage*100+32767);
               byte(lindata[7]):=lo(w);
               byte(lindata[8]):=hi(w);
               SendReply;
             end;
           $14,$34,$37,$39,$35,$3b:
             begin
               //FIXME
               FillByte(lindata[1],8,0);
               SendReply;
             end;
           $3d:
             if (nad=$01) or (nad=$7f) then begin //only reply if master request was for own nad or broadcast
               byte(lindata[1]):=$01;
               if (mrsid=$b8) and (nad=$01) then
               begin
                 byte(lindata[2]):=$03;
                 byte(lindata[3]):=$f8;
                 byte(lindata[4]):=FDiagReply1;
                 byte(lindata[5]):=FDIagReply1;
                 byte(lindata[6]):=$ff;
                 byte(lindata[7]):=$ff;
                 byte(lindata[8]):=$ff;
               end else if (mrsid=$b2) and (nad=$01) then
               begin
                 byte(lindata[2]):=$06;
                 byte(lindata[3]):=$f2;
                 byte(lindata[4]):=$01;
                 byte(lindata[5]):=FDIagReply2[1];
                 byte(lindata[6]):=FDIagReply2[2];
                 byte(lindata[7]):=FDIagReply2[3];
                 byte(lindata[8]):=$ff;
               end else if (mrsid=$b2) and (nad=$7f) then  //broadcast
               begin
                 Log('sending $3d broadcast reply');
                 byte(lindata[2]):=$06;
                 byte(lindata[3]):=$f2;
                 byte(lindata[4]):=$17; //supplier id $4617-> truma
                 byte(lindata[5]):=$46;
                 byte(lindata[6]):=$10; //function id $0310 -> combi diesel
                 byte(lindata[7]):=$03;
                 byte(lindata[8]):=$03; //tin variant
               end else
               begin
                 byte(lindata[2]):=$03;
                 byte(lindata[3]):=mrsid+64;
                 byte(lindata[4]):=0;
                 byte(lindata[5]):=0;
                 byte(lindata[6]):=$ff;
                 byte(lindata[7]):=$ff;
                 byte(lindata[8]):=$ff;
               end;
               SendReply;
             end;
           else
             begin
               framereceived:=true;
               checksumreceived:=false;
               for i:=1 to 8 do
                 if SerReadTimeout(FPort,c,1,50)<>1 then
                 begin
                   framereceived:=false;
                   break;
                 end else
                   byte(lindata[i]):=c;
               if framereceived then
                 checksumreceived:=SerReadTimeout(FPort,chksum,1,50)=1
               else
                 Log('frame '+inttohex(id,2)+' not received');
               if checksumreceived then
               begin
                 expected:=GetChecksum(pid,lindata);
                 if  chksum<>expected then
                 begin
                   Log(format('bad checksum id %.2x, expected %.2x received %.2x',[id,expected,chksum]));
                   framereceived:=false;
                 end;
               end;
               if framereceived and checksumreceived then
               begin
                 case id of
                    $3,$4:
                      begin
                        FSetPointWater:=id=4;
                        NewSetPointReceived:=DecodeTempKelvin(byte(lindata[1]),byte(lindata[2]));
                        if NewSetPointReceived<>FSetPointReceived[FSetPointWater] then
                        begin
                          FSetPointReceived[FSetPointWater]:=NewSetPointReceived;
                          Synchronize(@NotifySetpoint)
                        end
                      end;
                    $5: //energy selection
                      begin
                      end;
                    $6: //power limit
                      begin
                      end;
                    $7: //fan
                      begin
                        NewFan:=byte(lindata[1]);
                        if NewFan<>FFan then
                        begin
                          FFan:=NewFan;
                          Synchronize(@NotifyFan);
                        end
                      end;
                    $3c:
                      begin
                        mrsid:=byte(lindata[3]);
                        nad:=byte(lindata[1]);
                        Log(format('received $3c with nad %x sid %x',[nad,mrsid]));
                        if (mrsid=$b8) and (nad=$01) then
                        begin
                          NewOnOff:=byte(lindata[6])<>0;
                          if NewOnOff<>FOnOff then
                          begin
                            FOnOff:=NewOnOff;
                            Synchronize(@NotifyOnoff);
                          end;
                        end
                      end;
                 end;

               end;
             end;
          end; //case id of
          Status:=WaitBreak;
        end;
    end;
  end;
  SerClose(FPort)
end;

constructor TTrumaReceiver.Create(ComPort: string;AOnSetpoint:TOnSetpoint; AOnFan:TOnFan; AOnOnoff: TOnOnoff; AOnMessage:TOnMessage );
begin
  FOnSetpoint:=AOnSetpoint;
  FOnFan:=AOnFan;
  FOnOnoff:=AOnOnoff;
  FOnMessage:=AOnMessage;
  FPort:=SerOpen(ComPort);
  if FPort>0 then
    SerSetParams(FPort,9600,8,NoneParity,1,[]);
  inherited create(false);
end;

end.

