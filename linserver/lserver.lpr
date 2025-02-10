program lserver;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, sysutils, blcksock, lin
  { you can add units after this };

{$R *.res}

var
  linmaster: TLinMaster;
  sock: TUDPBlockSocket;
  block, recvframe: String;
  len: SizeInt;
  debug: Boolean;
  i: Integer;

  function Serror(const m: String): boolean;
  begin
    result:=sock.LastError<>0;
    if result then
      Writeln('error ',m,': ',sock.LastErrorDesc);
  end;

begin
  if paramcount<1 then
  begin
    writeln('usage: lserver <comport>');
    exit;
  end;

  debug:=false;
  if paramcount>=2 then
    if paramstr(2)='debug' then
      debug:=true;

  linmaster:=TLinMaster.Create(paramstr(1),9600);
  if linmaster.LastErrorDesc<>'' then
  begin
    writeln(linmaster.LastErrorDesc);
    linmaster.free;
    exit;
  end;


  sock:=TUDPBlockSocket.Create;
  sock.bind('0.0.0.0','4617');
  if Serror('bind') then
  begin
    writeln('cannot bind to port 4617');
    sock.free;
    linmaster.free;
  end;

  linmaster.verbose:=true;
  writeln('ready');

  while true do
  begin
    block:=Sock.RecvPacket(-1);
    if Serror('receive') then
      continue;
    len:=length(block);
    if len<1 then
    begin
      writeln('command too short');
      continue
    end;
    case ord(block[1]) of
      1: //writeframe
        begin
          if len<3 then //at least frame id and one byte
          begin
            writeln('writeframe command too short');
            continue
          end;
          if debug then
          begin
            write(format(' writeframe id %.2x                 ',[ord(block[2])]));
            for i:=3 to len do
              write(format('%.2x ',[ord(block[i])]));
            writeln();
          end;
          if linmaster.WriteFrame(ord(block[2]),copy(block,3,8)) then
            sock.SendString(chr(0))
          else
            sock.SendString(chr(1));
          Serror('send');
        end;
      2://readframe
        begin
          if len<3 then //frameid+expected length
          begin
            writeln('readframe command too short');
            continue;
          end;
          if debug then
          begin
            writeln(format('  readframe id %.2x ',[ord(block[2])]));
          end;
          if Linmaster.ReadFrame(ord(block[2]),recvframe,ord(block[3])) then
            Sock.SendString(chr(0)+recvframe)
          else
            Sock.SendString(chr(1));
          Serror('send');
        end;
      10://ping
        sock.sendstring(chr(10))
    end;
  end;

end.


