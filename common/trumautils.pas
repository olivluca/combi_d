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

end.

