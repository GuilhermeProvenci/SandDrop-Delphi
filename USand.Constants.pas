unit USand.Constants;

interface

uses
  Vcl.Graphics;

const
  MatrixSize = 400;
  CellSize = 2;
  StepsPerFrame = 4;

  MAT_EMPTY = 0;
  MAT_SAND  = 1;
  MAT_WATER = 2;
  MAT_STONE = 3;

type
  TMatrixBuffer = TArray<Integer>;
  TColorBuffer = TArray<TColor>;
  PMatrixBuffer = PInteger;
  PColorBuffer = PColor;

implementation

end.
