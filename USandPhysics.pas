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

  // Multi-Column Rescue: searches current AND neighboring columns to eliminate vertical gaps
  procedure RescueParticle(PType: Integer; PColor: TColor; StartY: Integer);
  var
    RY, RIdx, RX, Side: Integer;
    TryLeft, TryRight: Boolean;
  begin
    // Pass 1: Current Column (Priority)
    for RY := StartY downto 0 do
    begin
      RIdx := RY * MatrixSize + X;
      if TInterlocked.CompareExchange(LNextSand[RIdx], PType, MAT_EMPTY) = MAT_EMPTY then
      begin
        LNextColors[RIdx] := PColor;
        Exit;
      end;
    end;
    
    // Pass 2: Neighbor Columns (The Lifeboat)
    // Randomize which side to try first to prevent vertical streaks
    TryLeft := (X > 0);
    TryRight := (X < MatrixSize - 1);
    
    for Side := 0 to 1 do
    begin
      if (Side = 0) and TryLeft and (Random(2) = 0) then
      begin
        RX := X - 1;
        for RY := StartY downto 0 do
        begin
          RIdx := RY * MatrixSize + RX;
          if TInterlocked.CompareExchange(LNextSand[RIdx], PType, MAT_EMPTY) = MAT_EMPTY then
          begin
            LNextColors[RIdx] := PColor;
            Exit;
          end;
        end;
        TryLeft := False;
      end;
      
      if (Side = 1) and TryRight then
      begin
        RX := X + 1;
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
      
      // If we didn't try left in the randomized first step, try it now
      if (Side = 1) and TryLeft then
      begin
        RX := X - 1;
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
  end;

  procedure PlacePreservedWater(TargetIdx: Integer; Color: TColor; FallbackY: Integer);
  begin
    if TInterlocked.CompareExchange(LNextSand[TargetIdx], MAT_WATER, MAT_EMPTY) = MAT_EMPTY then
      LNextColors[TargetIdx] := Color
    else
      RescueParticle(MAT_WATER, Color, FallbackY);
  end;

begin
  LSand := PIntArray(PSand);
  LNextSand := PIntArray(PNextSand);
  LColors := PColorArray(PColors);
  LNextColors := PColorArray(PNextColors);

  // PRE-PASS: Statics (Stones)
  for Y := 0 to MatrixSize - 1 do
  begin
    CurrentIdx := Y * MatrixSize + X;
    if LSand[CurrentIdx] = MAT_STONE then
    begin
      LNextSand[CurrentIdx] := MAT_STONE;
      LNextColors[CurrentIdx] := LColors[CurrentIdx];
    end;
  end;

  // DYNAMIC PASS
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
          // Double-check swap for mass conservation
          if TInterlocked.CompareExchange(LNextSand[BottomIdx], MAT_SAND, MAT_WATER) = MAT_WATER then
          begin
            PlacePreservedWater(CurrentIdx, clSkyBlue, Y);
            LNextColors[BottomIdx] := LColors[CurrentIdx];
            CanMove := 1;
            FoundSpot := True;
          end;
        end;
      end;

      // 2. DIAGONAL (Randomized direction)
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

    // 3. WATER LEVELING (Optimized)
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
            // Optimization: Skip checking if it's fluid, only stop for solids
            if (LSand[Y * MatrixSize + NewX] = MAT_STONE) or (LSand[Y * MatrixSize + NewX] = MAT_SAND) then Break;
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

    // 4. STAY PUT (Final Fallback)
    if not FoundSpot then
    begin
      if TInterlocked.CompareExchange(LNextSand[CurrentIdx], mat, MAT_EMPTY) = MAT_EMPTY then
        LNextColors[CurrentIdx] := LColors[CurrentIdx]
      else
        // If our cell was stolen by a falling particle, we MUST be rescued (can look sideways now)
        RescueParticle(mat, LColors[CurrentIdx], Y);
    end;
  end;
end;

end.
