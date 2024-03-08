unit lin;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, serial
  {$IFDEF WINDOWS}
  ,windows
  {$ELSE}
  ,termio, baseunix, unix
  {$ENDIF}
;

type

  { TLinMaster }

  TLinMaster = class
    private
      FPort:TSerialHandle;
      FPortname:string;
      FBaud:integer;
      FLastErrorDesc:string;
      FVerbose:boolean;
      FFakeReceive:boolean;
      function GetProtectedId(const id:Uint8):Uint8;
      function GetChecksum(const pid:UInt8; const data:String):Uint8;
      procedure WriteBreak;
      procedure StartTransmission(const pid:Uint8);
      function NextByte:byte;
      function WaitingData:integer;
      procedure SendByte(const b:byte);
    public
      Constructor Create(const port:string; const baudrate:integer);
      function ReadFrame(const id:UInt8; out data:string; const expectedlen:Uint8=0):boolean;
      function WriteFrame(const id:Uint8; const data:string):boolean;
      //function WriteFrameClassic(const id:Uint8; const data:string):boolean;
      property LastErrorDesc:string read FLastErrorDesc;
      property Verbose:boolean read FVerbose write FVerbose;
      property FakeReceive:boolean read FFakeReceive write FFakeReceive;
  end;

implementation

{ TLinMaster }

function bitRead(const by,bit:uint8):uint8;
begin
   if (by and (1 shl bit)) <>0 then
     result:=1
   else
     result:=0;
end;

function TLinMaster.GetProtectedId(const id: Uint8): Uint8;
var
  p0,p1: uint8;
begin
   p0 := bitRead(ID, 0) xor  bitRead(ID, 1) xor bitRead(ID, 2) xor bitRead(ID, 4);
   p1 := not (bitRead(ID, 1) xor bitRead(ID, 3) xor bitRead(ID, 4) xor bitRead(ID, 5));
   result:=(p1 shl 7) or (p0 shl 6) or (id and $3f);
end;

function TLinMaster.GetChecksum(const pid: UInt8; const data: String): Uint8;
var
  sum: UInt32;
  i: Integer;
begin
  sum:=pid;
  if (sum and $3f)>=$3c then
    sum:=0;
  for i:=1 to length(data) do
    sum:=sum+ord(data[i]);
  while (sum and $ff00) > 0 do
     sum:=(sum and $ff) + (sum shr 8);
  result:=not sum;
end;

procedure TLinMaster.WriteBreak;
var b:byte;
begin

  {$IFDEF WINDOWS}
  //en Linux no se puede controlar la duración del break
  SerBreak(FPort,1000 * 14 div FBaud, false);
   exit;
  {$ELSE}
  //reabre el puerto con la mitad del baud para hacer un break reconocible por el bus
  //(no puedo usar SerSetParams directamente, funciona solo una vez y después no hace nada)
  SerClose(FPort);
  Fport:=SerOpen(FPortname);
  SerSetParams(FPort,FBaud div 2, 8, NoneParity,1,[]);
  SendByte(0);
  SerDrain(Fport);
  //reabre el puerto a la velocidad normal
  SerClose(FPort);
  Fport:=SerOpen(FPortname);
  SerSetParams(FPort,FBaud, 8, NoneParity, 1, []);
  {$ENDIF}
end;

procedure TLinMaster.StartTransmission(const pid: Uint8);
begin
  WriteBreak;
  SendByte($55);
  SendByte(pid);
end;

function TLinMaster.NextByte: byte;
begin
  SerRead(FPort,result,1);
end;

//función no implementada en Serial
{$IFDEF WINDOWS}
function TLinMaster.WaitingData: integer;
var
  stat: TComStat;
  err: DWORD;
begin
  err := 0;
  if ClearCommError(FPort, err, @stat) then
  begin
    Result := stat.cbInQue;
  end
  else
  begin
    Result := 0;
  end;
end;
{$ELSE}
function TLinMaster.WaitingData: integer;
begin
  if fpIoctl(FPort, FIONREAD, @result)<0 then
    result:=0;
end;
{$ENDIF}

procedure TLinMaster.SendByte(const b: byte);
begin
  SerWrite(Fport,b,1);
end;

constructor TLinMaster.Create(const port: string; const baudrate: integer);
begin
  FLastErrorDesc:='';
  FBaud:=baudrate;
  FPortname:=port;
  FPort:=SerOpen(FPortname);
  if FPort<=0 then
  begin
    FLastErrorDesc:='port not found';
    exit;
  end;
  SerSetParams(FPort,FBaud,8,NoneParity,1,[]);
end;

function TLinMaster.ReadFrame(const id: UInt8; out data: string;
  const expectedlen: Uint8): boolean;
var
  pid, CalcChecksum: UInt8;
  stime: QWord;
  bytes_received, RecChecksum, i, x: Integer;
  buf: Byte;
  loc_data:array[0..8] of char;
begin
  pid:=GetProtectedId(id);
  if FFakeReceive then
  begin
    for x:=0 to 7 do
      loc_data[x]:=chr(Random(256));
    //FillByte(loc_data,sizeof(loc_data),id);
    setlength(data,8);
    move(loc_data,data[1],8);
    byte(loc_data[8]):=GetChecksum(pid,data);
    bytes_received:=9;
  end else
  begin
    StartTransmission(pid);
    SerDrain(FPort);
    if expectedlen>0 then
    begin
      stime:=GetTickCount64;
      while GetTickCount64-stime < 100 do
      begin
        if WaitingData>=expectedlen+4 then  //4 -> break, sync, pid, RecChecksum
          break;
        sleep(1);
      end;
    end else
      sleep(100);

    bytes_received:=-4;
    while WaitingData>0 do
    begin
      if bytes_received>8 then //9 bytes already received (8 data and checksum)
        break;
      buf:=NextByte;
      //write(bytes_received,':', inttohex(buf,2),' - ');
      case bytes_received of
        -4, //??
        -3, //break
        -2, //sync $55
        -1:  //pid
          begin
            case buf of
              $00: bytes_received:=-3;
              $55: bytes_received:=-2;
              else
                if buf=pid then
                  bytes_received:=-1
             end;
          end;
        else
        begin
          loc_data[bytes_received]:=chr(buf);
        end;
      end;
      bytes_received:=bytes_received+1;
    end;
    while WaitingData>0 do
      NextByte;
  end; //not FakeReceive
  if FVerbose then
    write(format(' --->>>>>> FID %.2Xh        = 55|%.2X|', [ID, PID]));

  if bytes_received>1 then
  begin
    RecChecksum:=ord(loc_data[bytes_received-1]);
    bytes_received:=bytes_received-1;
    setlength(data,bytes_received);
    move(loc_data,data[1],bytes_received);
    CalcChecksum :=GetChecksum(pid,data);
    result:=CalcChecksum=RecChecksum;
    if result then
      FLastErrorDesc:=''
    else
      FLastErrorDesc:='bad checksum, received '+IntToHex(RecChecksum,2)+' expected '+IntToHex(CalcChecksum,2);
    if FVerbose then
    begin
        for i:= 0 to bytes_received -1 do
          write(format('%.2X.', [ord(loc_data[i])]));
        write(format('|%.2X', [RecChecksum]));
        write(format(' bytes received %d',[bytes_received]));
        if FLastErrorDesc<>'' then
                write(' ',FLastErrorDesc);
        writeln('');
    end;
  end else
  begin
    FLastErrorDesc:='no bytes received';
    result:=false;
    if FVerbose then writeln('****** ',FLastErrorDesc)
  end;
end;

function TLinMaster.WriteFrame(const id: Uint8; const data: string): boolean;
var
  pid, checksum, ChkSumCalc: UInt8;
  RX_Sync, RX_ProtectedID: Byte;
  moreData: Boolean;
  loc_data:array[0..8] of char;
  slocdata:String;
  bytes_received, Checksum_received, i: Integer;
begin
  pid:=GetProtectedId(id);
  checksum:=GetChecksum(pid,data);
  StartTransmission(pid);
  for i:=1 to length(data) do
    SendByte(ord(data[i]));
  SendByte(checksum);
  SerDrain(FPort);
  sleep(20);
  // Read Break and discard
  if WaitingData>0 then
    NextByte;

  // Read Sync
  RX_Sync := $00;
  if WaitingData>0 then
      RX_Sync := NextByte;

  //Read PID
  RX_ProtectedID := $00;
  if WaitingData>0 then
      RX_ProtectedID := NextByte;
  // read DATA + CHKSUM
  moreData := false;
  bytes_received := 0;
  while WaitingData>0 do
  begin
      if bytes_received >= 8 + 1 + 4 then
      begin
          // receive max 9 Bytes = 8 Data + 1 Chksum
          moreData := true;
          break;
      end;
      // Receive Byte from Bus (Slave)
      loc_data[bytes_received] := chr(NextByte);
      bytes_received:=bytes_received+1;
  end;
  Checksum_received := ord(Loc_data[bytes_received - 1]);
  bytes_received:=bytes_received-1;
  SetLength(slocdata,bytes_received);
  move(loc_data,slocdata[1],bytes_received);

  // erase data in buffer, in case a 9th or 10th Byte was received
  while WaitingData>0 do
    NextByte;

  // use received PID  for verification
  ChkSumCalc := getChecksum(RX_ProtectedID, slocdata);

  if FVerbose then
  begin
      write(format(' <<<<<<--- FID %.2Xh (%.2X)   = %.2X|%.2X|', [ID, PID, RX_Sync, RX_ProtectedID]));
      for i := 0 to bytes_received-1 do
        write(format('%.2X ', [ord(loc_data[i])]));
      write(format('|%.2X ', [Checksum_received]));
      if Checksum_received <> ChkSumCalc then
          write(format('!= ChkSum calc %.2Xh| TX %.2Xh ', [ChkSumCalc, checksum]));
      if moreData then
          write('more Bytes available');
      writeln('');
  end;
  result:=Checksum_received = ChkSumCalc;
end;

end.

