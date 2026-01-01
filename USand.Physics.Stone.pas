unit USand.Physics.Stone;

interface

uses
  USand.Constants, USand.Physics.Types;

type
  TStoneBehavior = class
  public
    class procedure Process(
      CurrentIdx: Integer;
      LColors, LNextColors: PColorArray;
      LNextSand: PIntArray
    ); static;
  end;

implementation

class procedure TStoneBehavior.Process(
  CurrentIdx: Integer;
  LColors, LNextColors: PColorArray;
  LNextSand: PIntArray
);
begin
  LNextSand[CurrentIdx] := MAT_STONE;
  LNextColors[CurrentIdx] := LColors[CurrentIdx];
end;

end.
