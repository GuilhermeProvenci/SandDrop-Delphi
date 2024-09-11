object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object PaintBox1: TPaintBox
    Left = 0
    Top = 0
    Width = 624
    Height = 441
    Align = alClient
    ExplicitLeft = 40
    ExplicitTop = 200
    ExplicitWidth = 105
    ExplicitHeight = 105
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 560
    Top = 360
  end
end
