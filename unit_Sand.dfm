object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object PaintBox1: TPaintBox
    Left = 0
    Top = 0
    Width = 624
    Height = 371
    Align = alClient
    OnMouseMove = PaintBox1MouseMove
    OnPaint = PaintBox1Paint
    ExplicitLeft = 272
    ExplicitTop = 192
    ExplicitWidth = 105
    ExplicitHeight = 105
  end
  object tbrRadius: TTrackBar
    Left = 0
    Top = 371
    Width = 624
    Height = 45
    Hint = 'Raio do Pincel'
    Align = alBottom
    Max = 100
    Position = 5
    TabOrder = 0
    ExplicitLeft = 376
    ExplicitTop = 388
    ExplicitWidth = 150
  end
  object btnReset: TButton
    Left = 0
    Top = 416
    Width = 624
    Height = 25
    Align = alBottom
    Caption = 'Reset'
    TabOrder = 1
    OnClick = btnResetClick
    ExplicitTop = 422
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 576
    Top = 312
  end
end
