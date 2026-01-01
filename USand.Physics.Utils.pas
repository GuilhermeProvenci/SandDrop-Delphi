unit USand.Physics.Utils;

interface

uses
  System.SyncObjs, System.Math, Vcl.Graphics, USand.Constants, USand.Physics.Types;

type
  TPhysicsUtils = class
  public
    class procedure RescueParticle(
      X, StartY: Integer;
      PType: Integer; 
      PColor: TColor;
      LNextSand: PIntArray;
      LNextColors: PColorArray
    ); static;
    
    class procedure PlacePreservedWater(
      X, FallbackY, TargetIdx: Integer;
      Color: TColor;
      LNextSand: PIntArray;
      LNextColors: PColorArray
    ); static;
  end;

implementation

class procedure TPhysicsUtils.RescueParticle(
  X, StartY: Integer;
  PType: Integer; 
  PColor: TColor;
  LNextSand: PIntArray;
  LNextColors: PColorArray
);
var
  RY, RIdx, RX, Side: Integer;
begin
  // Pass 1: Current Column
  for RY := StartY downto 0 do
  begin
    RIdx := RY * MatrixSize + X;
    if TInterlocked.CompareExchange(LNextSand[RIdx], PType, MAT_EMPTY) = MAT_EMPTY then
    begin
      LNextColors[RIdx] := PColor;
      Exit;
    end;
  end;
  
  // Pass 2: Neighbor Columns
  for Side := 0 to 1 do
  begin
    RX := X + IfThen(Side = 0, -1, 1);
    if (RX < 0) or (RX >= MatrixSize) then Continue;
    
    for RY := StartY downto 0 do
    begin
      RIdx := RY * MatrixSize + RX;
      if TInterlocked.CompareExchange(LNextSand[RIdx], PType, MAT_EMPTY) = MAT_EMPTY then
      begin
        LNextColors[RIdx] := PColor;
        Exit;
      end;
    end;
  end;
end;

class procedure TPhysicsUtils.PlacePreservedWater(
  X, FallbackY, TargetIdx: Integer;
  Color: TColor;
  LNextSand: PIntArray;
  LNextColors: PColorArray
);
begin
  if TInterlocked.CompareExchange(LNextSand[TargetIdx], MAT_WATER, MAT_EMPTY) = MAT_EMPTY then
    LNextColors[TargetIdx] := Color
  else
    RescueParticle(X, FallbackY, MAT_WATER, Color, LNextSand, LNextColors);
end;

end.
