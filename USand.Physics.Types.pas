unit USand.Physics.Types;

interface

uses
  Vcl.Graphics, USand.Constants;

type
  PIntArray = ^TIntArray;
  TIntArray = array[0..MaxInt div 4 - 1] of Integer;
  PColorArray = ^TColorArray;
  TColorArray = array[0..MaxInt div 4 - 1] of TColor;

implementation

end.
