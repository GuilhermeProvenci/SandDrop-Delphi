unit USandPhysics;

interface

uses
  System.SysUtils, System.Math, System.SyncObjs, Winapi.Windows, Vcl.Graphics;

const
  MAT_EMPTY = 0;
  MAT_SAND  = 1;
  MAT_WATER = 2;
  MAT_STONE = 3;

type
  TSandPhysics = class
  public
    class procedure ProcessColumn(
      X, MatrixSize: Integer;
      PSand, PNextSand: PInteger;
      PColors, PNextColors: PColor;
      var CanMove: Integer
    );
  end;

implementation

class procedure TSandPhysics.ProcessColumn(
  X, MatrixSize: Integer;
  PSand, PNextSand: PInteger;
  PColors, PNextColors: PColor;
  var CanMove: Integer
);
type
  PIntArray = ^TIntArray;
  TIntArray = array[0..MaxInt div 4 - 1] of Integer;
  PColorArray = ^TColorArray;
  TColorArray = array[0..MaxInt div 4 - 1] of TColor;
var
  Y, CurrentIdx, BottomIdx, mat, targetMat, Offset, NewX, i: Integer;
  FoundSpot: Boolean;
  Dir: Integer;
  LSand, LNextSand: PIntArray;
  LColors, LNextColors: PColorArray;
  OldVal: Integer;

  // The Search-and-Rescue mechanism (Vertical Eruption)
  procedure RescueParticle(PType: Integer; PColor: TColor; StartY: Integer);
  var
    RY, RIdx: Integer;
  begin
    // Scan UPWARDS from StartY to find the first available hole in the next buffer
    for RY := StartY downto 0 do
    begin
      RIdx := RY * MatrixSize + X;
      if TInterlocked.CompareExchange(LNextSand[RIdx], PType, MAT_EMPTY) = MAT_EMPTY then
      begin
        LNextColors[RIdx] := PColor;
        Exit;
      end;
    end;
    
    // Extreme Emergency: search immediate neighbors if current column is completely full
    if (X > 0) then
    begin
      for RY := StartY downto 0 do
      begin
        RIdx := RY * MatrixSize + (X - 1);
        if TInterlocked.CompareExchange(LNextSand[RIdx], PType, MAT_EMPTY) = MAT_EMPTY then
        begin
          LNextColors[RIdx] := PColor;
          Exit;
        end;
      end;
    end;
    
    if (X < MatrixSize - 1) then
    begin
      for RY := StartY downto 0 do
      begin
        RIdx := RY * MatrixSize + (X + 1);
        if TInterlocked.CompareExchange(LNextSand[RIdx], PType, MAT_EMPTY) = MAT_EMPTY then
        begin
          LNextColors[RIdx] := PColor;
          Exit;
        end;
      end;
    end;
  end;

  procedure PlacePreservedWater(TargetIdx: Integer; Color: TColor; FallbackY: Integer);
  begin
    // Try original spot
    if TInterlocked.CompareExchange(LNextSand[TargetIdx], MAT_WATER, MAT_EMPTY) = MAT_EMPTY then
      LNextColors[TargetIdx] := Color
    else
      // Spot stolen by a neighbor! Trigger Rescue
      RescueParticle(MAT_WATER, Color, FallbackY);
  end;

begin
  LSand := PIntArray(PSand);
  LNextSand := PIntArray(PNextSand);
  LColors := PColorArray(PColors);
  LNextColors := PColorArray(PNextColors);

  // PASS 1: Reserve Stones (immobile anchors)
  for Y := 0 to MatrixSize - 1 do
  begin
    CurrentIdx := Y * MatrixSize + X;
    if LSand[CurrentIdx] = MAT_STONE then
    begin
      LNextSand[CurrentIdx] := MAT_STONE;
      LNextColors[CurrentIdx] := LColors[CurrentIdx];
    end;
  end;

  // PASS 2: Dynamic simulation
  for Y := MatrixSize - 1 downto 0 do
  begin
    CurrentIdx := Y * MatrixSize + X;
    mat := LSand[CurrentIdx];
    
    if (mat = MAT_EMPTY) or (mat = MAT_STONE) then Continue;

    FoundSpot := False;
    if Y < MatrixSize - 1 then
    begin
      // 1. VERTICAL FALL / DISPLACEMENT
      BottomIdx := (Y + 1) * MatrixSize + X;
      targetMat := LSand[BottomIdx];
      
      if (targetMat = MAT_EMPTY) or ((mat = MAT_SAND) and (targetMat = MAT_WATER)) then
      begin
        OldVal := TInterlocked.CompareExchange(LNextSand[BottomIdx], mat, MAT_EMPTY);
        if OldVal = MAT_EMPTY then
        begin
          LNextColors[BottomIdx] := LColors[CurrentIdx];
          CanMove := 1;
          FoundSpot := True;
        end
        else if (mat = MAT_SAND) and (OldVal = MAT_WATER) then
        begin
          // Strict swap: Sand takes Water's slot
          if TInterlocked.CompareExchange(LNextSand[BottomIdx], MAT_SAND, MAT_WATER) = MAT_WATER then
          begin
            PlacePreservedWater(CurrentIdx, clSkyBlue, Y);
            LNextColors[BottomIdx] := LColors[CurrentIdx];
            CanMove := 1;
            FoundSpot := True;
          end;
        end;
      end;

      // 2. DIAGONAL
      if not FoundSpot then
      begin
        if (mat <> MAT_SAND) or (targetMat <> MAT_WATER) or (Random(20) = 0) then
        begin
          Dir := IfThen(Random(2) = 0, -1, 1);
          for i := 0 to 1 do
          begin
            Offset := IfThen(i = 0, Dir, -Dir);
            NewX := X + Offset;
            if (NewX >= 0) and (NewX < MatrixSize) then
            begin
              BottomIdx := (Y + 1) * MatrixSize + NewX;
              targetMat := LSand[BottomIdx];
              if (targetMat = MAT_EMPTY) or ((mat = MAT_SAND) and (targetMat = MAT_WATER)) then
              begin
                OldVal := TInterlocked.CompareExchange(LNextSand[BottomIdx], mat, MAT_EMPTY);
                if OldVal = MAT_EMPTY then
                begin
                  LNextColors[BottomIdx] := LColors[CurrentIdx];
                  CanMove := 1;
                  FoundSpot := True;
                  Break;
                end
                else if (mat = MAT_SAND) and (OldVal = MAT_WATER) then
                begin
                  if TInterlocked.CompareExchange(LNextSand[BottomIdx], MAT_SAND, MAT_WATER) = MAT_WATER then
                  begin
                    PlacePreservedWater(CurrentIdx, clSkyBlue, Y);
                    LNextColors[BottomIdx] := LColors[CurrentIdx];
                    CanMove := 1;
                    FoundSpot := True;
                    Break;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;

    // 3. FLUID LEVELING
    if (mat = MAT_WATER) and (not FoundSpot) then
    begin
      Dir := IfThen(Random(2) = 0, 1, -1);
      for i := 0 to 1 do
      begin
        Offset := IfThen(i = 0, Dir, -Dir);
        NewX := X + Offset;
        while (Abs(NewX - X) <= 15) and (NewX >= 0) and (NewX < MatrixSize) do
        begin
          if LSand[Y * MatrixSize + NewX] <> MAT_EMPTY then
          begin
            if LSand[Y * MatrixSize + NewX] <> MAT_WATER then Break;
          end
          else
          begin
            if TInterlocked.CompareExchange(LNextSand[Y * MatrixSize + NewX], MAT_WATER, MAT_EMPTY) = MAT_EMPTY then
            begin
              LNextColors[Y * MatrixSize + NewX] := LColors[CurrentIdx];
              CanMove := 1;
              FoundSpot := True;
              Break;
            end;
          end;
          NewX := NewX + Offset;
        end;
        if FoundSpot then Break;
      end;
    end;

    // 4. STAY PUT (With Rescue)
    if not FoundSpot then
    begin
      if TInterlocked.CompareExchange(LNextSand[CurrentIdx], mat, MAT_EMPTY) = MAT_EMPTY then
        LNextColors[CurrentIdx] := LColors[CurrentIdx]
      else
        // If our current spot was stolen by a falling/moving particle, we MUST be rescued
        RescueParticle(mat, LColors[CurrentIdx], Y);
    end;
  end;
end;

end.
