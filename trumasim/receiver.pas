unit receiver;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,trumautils,Serial,StrUtils,Dialogs;

const ReplayMin = $33;
      ReplayMax = $3b;

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
    FReplyToFrame14:boolean;
    FOnSetpoint:TOnSetpoint;
    FOnFan:TOnFan;
    FOnOnoff:TOnOnoff;
    FOnMessage:TOnMessage;
    FReplay:array[ReplayMin..ReplayMax] of TStringList;
    FReplayIndex:array[ReplayMin..ReplayMax] of integer;
    FPlaying:boolean;
    FReplayOk:boolean;
    function GetOpened: boolean;
    procedure Log(const m: string);
    procedure NotifySetpoint;
    procedure NotifyFan;
    procedure NotifyOnOff;
    procedure ReadReplay(const f: string);
    procedure Synclog;
  protected
    Procedure Execute;override;
  public
    Constructor Create(ComPort,Replay:string; AOnSetpoint:TOnSetpoint; AOnFan:TOnFan; AOnOnoff: TOnOnoff; AOnMessage:TOnMessage);
    destructor Destroy;override;
    property Opened:boolean read GetOpened;
    //don't really care for atomicity
    property StatusByte:byte read FStatusByte write FStatusByte;
    property Temperature:extended read FTemperature write FTemperature;
    property TemperatureWater:extended read FTemperatureWater write FTemperatureWater;
    property Voltage:extended read FVoltage write FVoltage;
    property DiagReply1:byte read FDiagReply1 write FDiagReply1;
    property DiagReply2:TDiagReply2 read FDiagReply2 write FDiagReply2;
    property ReplyToFrame14:boolean read FReplyToFrame14 write FReplyToFrame14;
    procedure RestartReplay;
    property Playing:boolean read FPlaying write FPlaying;
    property ReplayOk:boolean read FReplayOk;
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
  mrsid,nad, b2func:byte;
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
        //Log('not receiving');
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
           ReplayMin..ReplayMax:
             begin
               if not FReplayOk or (FReplay[id].Count=0) then
                 FillByte(lindata[1],8,0)
               else
               begin
                 Move(FReplay[id][FReplayIndex[id]][1],lindata[1],8);
                 if FPlaying then
                 begin
                   inc(FReplayIndex[id]);
                   if FReplayIndex[id]>=FReplay[id].count then
                     FReplayIndex[id]:=0;
                 end;
               end;
               SendReply;
             end;
           $14,$15,$1a:
             begin
               if (id<>$14) or FReplyToFrame14 then
               begin
                 FillByte(lindata[1],8,0);
                 SendReply;
               end;
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
                 case b2func of
                   $20://read software version
                     begin
                       byte(lindata[2]):=$04;
                       byte(lindata[3]):=$f2;
                       byte(lindata[4]):=$06; //sw version major
                       byte(lindata[5]):=$00; //sw version minor;
                       byte(lindata[6]):=$02; //sw version bugfix
                       byte(lindata[7]):=$ff;
                       byte(lindata[8]):=$ff;
                     end;
                   $22://read tin version
                     begin
                       byte(lindata[2]):=$04;
                       byte(lindata[3]):=$f2;
                       byte(lindata[4]):=$03; //sw version major
                       byte(lindata[5]):=$00; //sw version minor;
                       byte(lindata[6]):=$00; //sw version bugfix
                       byte(lindata[7]):=$ff;
                       byte(lindata[8]):=$ff;
                     end;
                   $23: //get error code
                     begin
                       byte(lindata[2]):=$06;
                       byte(lindata[3]):=$f2;
                       byte(lindata[4]):=$01;
                       byte(lindata[5]):=FDIagReply2[1];
                       byte(lindata[6]):=FDIagReply2[2];
                       byte(lindata[7]):=FDIagReply2[3];
                       byte(lindata[8]):=$ff;
                     end;
                  end
               end else if (mrsid=$b2) and (nad=$7f) then  //broadcast
               begin
                 //Log('sending $3d broadcast reply');
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
               nad:=0;
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
                        //Log(format('received $3c with nad %x sid %x',[nad,mrsid]));
                        if (mrsid=$b8) and (nad=$01) then
                        begin
                          NewOnOff:=byte(lindata[6])<>0;
                          if NewOnOff<>FOnOff then
                          begin
                            FOnOff:=NewOnOff;
                            Synchronize(@NotifyOnoff);
                          end;
                        end;
                        if (mrsid=$b2) and (nad=$01) then
                        begin
                          b2func:=byte(lindata[4]);
                          //log('received b2func '+INtToHex(b2func,2))
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

constructor TTrumaReceiver.Create(ComPort,Replay: string;AOnSetpoint:TOnSetpoint; AOnFan:TOnFan; AOnOnoff: TOnOnoff; AOnMessage:TOnMessage );
var
  fr: Integer;
begin
  for fr:=ReplayMin to ReplayMax do
    FReplay[fr]:=TStringList.Create;
  FOnSetpoint:=AOnSetpoint;
  FOnFan:=AOnFan;
  FOnOnoff:=AOnOnoff;
  FOnMessage:=AOnMessage;
  FPort:=SerOpen(ComPort);
  if FPort>0 then
    SerSetParams(FPort,9600,8,NoneParity,1,[]);
  if Replay<>'' then
  try
    ReadReplay(Replay);
    FPlaying:=true;
    FReplayOk:=true;
  except
    on E:exception do
      MessageDlg('Error reading replay data','Error reading replay data: '+E.Message,mtError,[mbOk],0);
  end;
  inherited create(false);
end;

procedure TTrumaReceiver.ReadReplay(const f:string);
const prefix=' --->>>>>> FID ';
var fin:textfile;
  line, sid, frame:string;
  index, i: Integer;
  b, id: LongInt;
begin
  assignfile(fin,f);
  reset(fin);
  while not eof(fin) do
  begin
    readln(fin,line);
    if pos('no bytes received',line)>0 then
      continue;
    if not line.startswith(prefix) then
      continue;
    sid:='$'+copy(line,length(prefix)+1,2);
    id:=StrToIntDef(sid,-1);
    if (id<ReplayMin) or (id>ReplayMax) then
      continue;
    line:=ExtractWord(3,line,['|']);
    frame:='';
    index:=1;
    for i:=1 to 8 do
    begin
      b:=StrToIntDef('$'+copy(line,index,2),-1);
      if (b<0) or (b>255) then
        break;
      frame:=frame+chr(b);
      index:=index+3;
    end;
    if length(frame)<>8 then
      continue;
    FReplay[id].Add(frame);
  end;
  closefile(fin);
end;

destructor TTrumaReceiver.Destroy;
var fr:integer;
begin
  for fr:=ReplayMin to ReplayMax do
    FReplay[fr].free;
  inherited Destroy;
end;

procedure TTrumaReceiver.RestartReplay;
var
  fr: Integer;
begin
  for fr:=ReplayMin to ReplayMax do
    FReplayIndex[fr]:=0;
end;

end.

