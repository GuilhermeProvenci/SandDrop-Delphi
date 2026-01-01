unit USand.Physics;

interface

uses
  System.SysUtils, System.Math, System.SyncObjs, Winapi.Windows, Vcl.Graphics,
  USand.Constants;

type
  TSandPhysics = class
  public
    class procedure ProcessColumn(
      X: Integer;
      PSand, PNextSand: PMatrixBuffer;
      PColors, PNextColors: PColorBuffer;
      var CanMove: Integer
    );
  end;

implementation

class procedure TSandPhysics.ProcessColumn(
  X: Integer;
  PSand, PNextSand: PMatrixBuffer;
  PColors, PNextColors: PColorBuffer;
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

  procedure RescueParticle(PType: Integer; PColor: TColor; StartY: Integer);
  var
    RY, RIdx, RX, Side: Integer;
    TryLeft, TryRight: Boolean;
  begin
    for RY := StartY downto 0 do
    begin
      RIdx := RY * MatrixSize + X;
      if TInterlocked.CompareExchange(LNextSand[RIdx], PType, MAT_EMPTY) = MAT_EMPTY then
      begin
        LNextColors[RIdx] := PColor;
        Exit;
      end;
    end;
    
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

  for Y := 0 to MatrixSize - 1 do
  begin
    CurrentIdx := Y * MatrixSize + X;
    if LSand[CurrentIdx] = MAT_STONE then
    begin
      LNextSand[CurrentIdx] := MAT_STONE;
      LNextColors[CurrentIdx] := LColors[CurrentIdx];
    end;
  end;

  for Y := MatrixSize - 1 downto 0 do
  begin
    CurrentIdx := Y * MatrixSize + X;
    mat := LSand[CurrentIdx];
    
    if (mat = MAT_EMPTY) or (mat = MAT_STONE) then Continue;

    FoundSpot := False;
    if Y < MatrixSize - 1 then
    begin
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
          if TInterlocked.CompareExchange(LNextSand[BottomIdx], MAT_SAND, MAT_WATER) = MAT_WATER then
          begin
            PlacePreservedWater(CurrentIdx, clSkyBlue, Y);
            LNextColors[BottomIdx] := LColors[CurrentIdx];
            CanMove := 1;
            FoundSpot := True;
          end;
        end;
      end;

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

    if not FoundSpot then
    begin
      if TInterlocked.CompareExchange(LNextSand[CurrentIdx], mat, MAT_EMPTY) = MAT_EMPTY then
        LNextColors[CurrentIdx] := LColors[CurrentIdx]
      else
        RescueParticle(mat, LColors[CurrentIdx], Y);
    end;
  end;
end;

end.
