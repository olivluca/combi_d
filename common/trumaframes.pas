unit trumaframes;

{$mode ObjFPC}{$H+}
{$PackRecords C}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils,Math;

type

  { TFrame14 }


  TFrame14 = bitpacked record
  private
    function GetRoomTargetTemperature: extended;
    function GetRoomTemperature: extended;
    function GetWaterTargetTemperature: extended;
  public
    _RoomTemperature:word;
    _RoomTargetTemperature: word;
    _WaterTargetTemperature: word;
    _EnergySelection: byte;
    _Spare: byte;
    property RoomTemperture:extended read GetRoomTemperature;
    property RoomTargetTemperature:extended read GetRoomTargetTemperature;
    property WaterTargetTemperature:extended read GetWaterTargetTemperature;
  end;

  { TFrame16 }

  TFrame16 = bitpacked record
  private
    function GetBatteryVoltage: extended;
    function GetRoomTemperature: extended;
    function GetWaterTemperature: extended;
  public
    FixInfo:byte;
    Easi:boolean;
    Supply220:boolean;
    Window:boolean;
    RoomDemand:boolean;
    WaterDemand:boolean;
    Bit13:boolean;
    Bit14:boolean;
    Error:boolean;
    _RoomTemperature:word;
    _WaterTemperature:word;
    _BatteryVoltage:word;
    property RoomTemperature:extended read GetRoomTemperature;
    property WaterTemperature:extended read GetWaterTemperature;
    property BatteryVoltage:extended read GetBatteryVoltage;
  end;

  { TFrame34 }

  TFrame34 = bitpacked record
  private
    function GetEbtMode: string;
    function GetOperationTime: integer;
  public
    Event2:byte;
    _OperationTime:packed array[0..2] of byte;
    BtMode:byte;
    StatusFlags4:byte;
    _EbtMode:byte;
    RelayK2:boolean;
    RelayK1:Boolean;
    RelayK3:boolean;
    bit3:boolean;
    bit4:boolean;
    bit5:boolean;
    bit6:boolean;
    bit7:boolean;
    property OperationTime:integer read GetOperationTime;
    property EbtMode:string read GetEbtMode;
  end;

  { TFrame39 }

  TFrame39 = bitpacked record
  private
    function GetExhaustTemperature: extended;
    function GetFlameTemperature: extended;
    function GetPumpFrequency: extended;
  public
    _ExhaustTemperature:word;
    ExhaustActualGradient:word;
    ExhaustGradient:word;
    _FlameTemperature:byte;
    _PumpFrequency:byte;
    property ExhaustTemperature:extended read GetExhaustTemperature;
    property PumpFrequency:extended read GetPumpFrequency;
    property FlameTemperature:extended read GetFlameTemperature;
  end;

  { TFrame35 }

  TFrame35 = bitpacked record
  private
    function GetBurnerFanVoltage: extended;
    function GetBurnerStatus: string;
    function GetHydronicFlame: string;
    function GetHydronicState: string;
    function GetGlowPlugStatus: string;
  public
    BatteryVoltageHydronic:byte;
    ConditionNrHydronic:byte;
    _AV3_Hydronic:byte;
    _EVENT0_Hydronic:byte;
    SafetyTime_Hydronic:byte;
    StateTime_Hydronic:byte;
    _BurnerFanVoltage:byte;
    _AV2_Hydronic:byte;
    property BurnerStatus:string read GetBurnerStatus;
    property GlowPlugStatus:string read GetGlowPlugStatus;
    property BurnerFanVoltage:extended read GetBurnerFanVoltage;
    property HydronicState:string read GetHydronicState;
    property HydronicFlame:string read GetHydronicFlame;
  end;

  { TFrame3b }

  TFrame3b = bitpacked record
  private
    function GetBattery: extended;
    function GetCircAirMotorCurrent: extended;
    function GetExtractorFanRpm: integer;
    function GetPumpStatus: string;
  public
    SoftwareVersion_Tech:byte;
    CurrentErrorTruma:byte;
    CurrenErrorHydronic:byte;
    _Battery:byte;
    Hyd_power_level:byte;
    CircAirMotor_Setpoint:byte;
    _I_ULM_Actual:byte;
    _ExtractorFanRpm:byte;
    property ExtractorFanRpm:integer read GetExtractorFanRpm;
    property Battery:extended read GetBattery;
    property PumpStatus:string read GetPumpStatus;
    property CircAirMotorCurrent:extended read GetCircAirMotorCurrent;
  end;

implementation


function GetTemperature(RawValue:word):extended;
begin
  result:=LeToN(RawValue)*0.1-273;
end;

{ TFrame3b }

function TFrame3b.GetBattery: extended;
begin
  result:=_Battery*0.1;
end;

function TFrame3b.GetCircAirMotorCurrent: extended;
begin
  result:=_I_ULM_Actual/10.0;
end;

function TFrame3b.GetExtractorFanRpm: integer;
begin
  if _ExtractorFanRpm=0 then
  begin
    result:=0;
    exit;
  end;
  result:=Round(58594.0 / _ExtractorFanRpm);
  if result<260 then
    result:=0;
end;

function TFrame3b.GetPumpStatus: string;
begin
  if CurrenErrorHydronic=48 then
    result:='open'
  else
    result:='closed';
end;

{ TFrame35 }

function TFrame35.GetBurnerFanVoltage: extended;
begin
  result:=_BurnerFanVoltage*0.1;
end;

function TFrame35.GetBurnerStatus: string;
begin
  case _AV3_Hydronic and $2f of
       0: result:='no info';
       1: result:='blow cold';
       2: result:='level high';
       4: result:='level middle';
       8: result:='level low';
       $10: result:='system check';
       $20: result:='run after';
       else result:='?'+inttohex(_AV3_Hydronic,2)
  end;
end;

function TFrame35.GetHydronicFlame: string;
begin
  if _EVENT0_Hydronic and $10 = 0 then
    result:='off'
  else
    result:='on';
end;

function TFrame35.GetHydronicState: string;
begin
  if _EVENT0_Hydronic and 2 =0  then
    result:='off'
  else
    result:='on'
end;

function TFrame35.GetGlowPlugStatus: string;
begin
  case _AV2_Hydronic and $0c of
       0: result:='no voltage';
       4: result:='start';
       8: result:='restart';
       else result:='?'+inttohex(_AV2_Hydronic,2);
  end;

end;

{ TFrame39 }

function TFrame39.GetExhaustTemperature: extended;
begin
  result:=LeToN(_ExhaustTemperature)*0.1;
end;

function TFrame39.GetFlameTemperature: extended;
begin
  result:=power(_FlameTemperature,3.0)* 1.8602209820528515E-05 + Power(_FlameTemperature, 2.0) * -0.0004895309102721512 + _FlameTemperature * 1.4470709562301636 -65.64685821533203;
end;

function TFrame39.GetPumpFrequency: extended;
begin
   result:=_PumpFrequency/25.0;
end;

{ TFrame34 }

function TFrame34.GetEbtMode: string;
begin
  case _EbtMode of
     0: result:='Electric 1800W';
     1: result:='Electric 900W';
     2: result:='Gas/Diesel';
     3: result:='Mixed 900W';
     4: result:='Mixed 1800W';
     else result:='?'+inttostr(_EbtMode);
  end;
end;

function TFrame34.GetOperationTime: integer;
begin
  result:=_OperationTime[0]*65536+_OperationTime[1]*256+_OperationTime[2];
end;

{ TFrame14 }

function TFrame14.GetRoomTargetTemperature: extended;
begin
  result:=GetTemperature(_RoomTargetTemperature);
end;

function TFrame14.GetRoomTemperature: extended;
begin
  result:=GetTemperature(_RoomTemperature);
end;

function TFrame14.GetWaterTargetTemperature: extended;
begin
  result:=GetTemperature(_WaterTargetTemperature);
end;

{ TFrame16 }

function TFrame16.GetBatteryVoltage: extended;
begin
  result:=LeToN(_BatteryVoltage)*0.01-327.67
end;

function TFrame16.GetRoomTemperature: extended;
begin
  result:=GetTemperature(_RoomTemperature);
end;

function TFrame16.GetWaterTemperature: extended;
begin
  result:=GetTemperature(_WaterTemperature);
end;

end.

