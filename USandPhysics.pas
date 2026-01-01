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
begin
  LSand := PIntArray(PSand);
  LNextSand := PIntArray(PNextSand);
  LColors := PColorArray(PColors);
  LNextColors := PColorArray(PNextColors);

  for Y := MatrixSize - 1 downto 0 do
  begin
    CurrentIdx := Y * MatrixSize + X;
    mat := LSand[CurrentIdx];
    
    if mat = MAT_EMPTY then Continue;

    // STONE: Static and batch copied (no atomic needed if strictly in-place)
    if mat = MAT_STONE then
    begin
      if LNextSand[CurrentIdx] = MAT_EMPTY then
      begin
        LNextSand[CurrentIdx] := MAT_STONE;
        LNextColors[CurrentIdx] := LColors[CurrentIdx];
      end;
      Continue;
    end;

    FoundSpot := False;
    if Y < MatrixSize - 1 then
    begin
      BottomIdx := (Y + 1) * MatrixSize + X;
      targetMat := LSand[BottomIdx];
      
      // 1. VERTICAL
      if (targetMat = MAT_EMPTY) or ((mat = MAT_SAND) and (targetMat = MAT_WATER)) then
      begin
        if TInterlocked.CompareExchange(LNextSand[BottomIdx], mat, MAT_EMPTY) = MAT_EMPTY then
        begin
          if (mat = MAT_SAND) and (targetMat = MAT_WATER) then
          begin
            if TInterlocked.CompareExchange(LNextSand[CurrentIdx], MAT_WATER, MAT_EMPTY) = MAT_EMPTY then
              LNextColors[CurrentIdx] := clSkyBlue;
          end;
          LNextColors[BottomIdx] := LColors[CurrentIdx];
          CanMove := 1;
          FoundSpot := True;
        end
        else if (mat = MAT_SAND) and (TInterlocked.CompareExchange(LNextSand[BottomIdx], MAT_SAND, MAT_WATER) = MAT_WATER) then
        begin
           if TInterlocked.CompareExchange(LNextSand[CurrentIdx], MAT_WATER, MAT_EMPTY) = MAT_EMPTY then
             LNextColors[CurrentIdx] := clSkyBlue;
           LNextColors[BottomIdx] := LColors[CurrentIdx];
           CanMove := 1;
           FoundSpot := True;
        end;
      end;

      // 2. DIAGONAL
      if not FoundSpot then
      begin
        // 5% chance of diagonal for sand in water
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
                if TInterlocked.CompareExchange(LNextSand[BottomIdx], mat, MAT_EMPTY) = MAT_EMPTY then
                begin
                  if (mat = MAT_SAND) and (targetMat = MAT_WATER) then
                  begin
                    if TInterlocked.CompareExchange(LNextSand[CurrentIdx], MAT_WATER, MAT_EMPTY) = MAT_EMPTY then
                      LNextColors[CurrentIdx] := clSkyBlue;
                  end;
                  LNextColors[BottomIdx] := LColors[CurrentIdx];
                  CanMove := 1;
                  FoundSpot := True;
                  Break;
                end
                else if (mat = MAT_SAND) and (TInterlocked.CompareExchange(LNextSand[BottomIdx], MAT_SAND, MAT_WATER) = MAT_WATER) then
                begin
                   if TInterlocked.CompareExchange(LNextSand[CurrentIdx], MAT_WATER, MAT_EMPTY) = MAT_EMPTY then
                     LNextColors[CurrentIdx] := clSkyBlue;
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

    // 3. AGGRESSIVE WATER LEVELING
    if (mat = MAT_WATER) and (not FoundSpot) then
    begin
      Dir := IfThen(Random(2) = 0, 1, -1);
      for i := 0 to 1 do
      begin
        Offset := IfThen(i = 0, Dir, -Dir);
        NewX := X + Offset;
        // Optimized search limit
        while (Abs(NewX - X) <= 15) and (NewX >= 0) and (NewX < MatrixSize) do
        begin
          if LSand[Y * MatrixSize + NewX] <> MAT_EMPTY then
          begin
            // Optimization: Only stop if it's Stone or Sand (don't stop for other water)
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

    // 4. STAY PUT
    if not FoundSpot then
    begin
      // Atomic check to avoid overwriting evicted water
      if TInterlocked.CompareExchange(LNextSand[CurrentIdx], mat, MAT_EMPTY) = MAT_EMPTY then
        LNextColors[CurrentIdx] := LColors[CurrentIdx];
    end;
  end;
end;

end.
