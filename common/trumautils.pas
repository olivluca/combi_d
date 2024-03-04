unit trumautils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TBoilerMode = (BoilerOff, BoilerEco, BoilerHot, BoilerBoost);
const
  BoilerTemp:array[TBoilerMode] of integer = (0,40,55,60);
type
  TFanMode = (FanOff, FanEco, FanHigh, FanOn);


type
  TEnergyPriority = (EpNone, EpFuel, EpBothPrioElectro, EpBothPrioFuel);

  TEnergySelection = (EsGasDiesel, EsMixed900, EsMixed1800, EsElectro900, EsElectro1800);

const
  EnergyPriorities : array[TEnergySelection] of TEnergyPriority = (EpFuel,EpBothPrioFuel,EpBothPrioFuel, EpBothPrioElectro, EpBothPrioElectro);
  EnergyPowerLimit : array[TEnergySelection] of word = (0, 900, 1800, 900, 1800);

  function GetChecksum(const pid:UInt8; const data:String):Uint8;
  function HeaterCommand(temperature:extended):string;
  function BoilerCommand(mode:TBoilerMode):string;
  function EnergySelect(const selection:TEnergySelection):string;
  function SetPowerLimit(const selection:TEnergySelection):string;
  function Fan(mode:byte):string;
  function DiagFrame(diagCycle:integer;onoff:boolean):string;

  function DecodeTempKelvin(const b1,b2:byte):Extended;
  function DecodeVoltage(const b1,b2:byte):Extended;
  procedure EncodeTempKelvin(const t:extended; out b1,b2:byte);

implementation

function GetChecksum(const pid:UInt8; const data:String):Uint8;
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

function EnergySelect(const selection:TEnergySelection): String;
begin
  setlength(result,8);
  FillByte(result[1],8,$FF);
  byte(result[1]):=ord(EnergyPriorities[selection]);
end;

function SetPowerLimit(const selection:TEnergySelection): String;
var
  w: Word;
begin
  setlength(result,8);
  FillByte(result[1],8,$FF);
  w:=EnergyPowerLimit[selection];
  Byte(result[1]):=lo(w);
  Byte(result[2]):=hi(w);
end;

function Fan(mode:byte):String;
begin
  setlength(result,8);
  FillByte(result[1],8,$FF);
  byte(result[1]):=mode or $e0;
  byte(result[2]):=$fe;
end;

function DiagFrame(diagCycle:integer; onoff: boolean): String;
const DiagFrames:array[1..2] of array[0..7] of byte = (
  ($01, $04, $B8, $10, $03, $00, $00, $FF),     //el ultimo byte antes del FF FF debería se 01 con alguna función activa?
  ($01, $06, $B2, $23, $17, $46, $10, $03)
  );
begin
  writeln('----- diagCycle ',diagCycle);
  SetLength(result,8);
  move(DiagFrames[diagCycle][0],result[1],8);
  if onoff and (diagCycle=1) then
  begin
    byte(result[6]):=$01;
    byte(result[7]):=$ff;
  end;
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

end.

