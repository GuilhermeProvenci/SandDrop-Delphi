object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 709
  ClientWidth = 958
  Color = clBtnFace
  DoubleBuffered = True
  DoubleBufferedMode = dbmRequested
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object tbrRadius: TTrackBar
    Left = 0
    Top = 614
    Width = 958
    Height = 45
    Hint = 'Raio do Pincel'
    Align = alBottom
    Max = 100
    Position = 5
    TabOrder = 0
    ExplicitTop = 346
    ExplicitWidth = 624
  end
  object btnReset: TButton
    Left = 0
    Top = 659
    Width = 958
    Height = 25
    Align = alBottom
    Caption = 'Reset'
    TabOrder = 1
    OnClick = btnResetClick
    ExplicitTop = 391
    ExplicitWidth = 624
  end
  object btnRain: TButton
    Left = 0
    Top = 684
    Width = 958
    Height = 25
    Align = alBottom
    Caption = 'Rain'
    TabOrder = 2
    OnClick = btnRainClick
    ExplicitTop = 416
    ExplicitWidth = 624
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 958
    Height = 614
    Align = alClient
    DoubleBufferedMode = dbmRequested
    TabOrder = 3
    ExplicitLeft = 392
    ExplicitTop = 352
    ExplicitWidth = 185
    ExplicitHeight = 41
    object PaintBox1: TPaintBox
      Left = 1
      Top = 1
      Width = 956
      Height = 612
      Margins.Left = 10
      Margins.Top = 10
      Margins.Right = 10
      Margins.Bottom = 10
      Align = alClient
      OnMouseMove = PaintBox1MouseMove
      OnPaint = PaintBox1Paint
      ExplicitLeft = 272
      ExplicitTop = 192
      ExplicitWidth = 105
      ExplicitHeight = 105
    end
    object lblFPS: TLabel
      Left = 10
      Top = 10
      Width = 50
      Height = 20
      Caption = 'FPS: 0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = True
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 16
    OnTimer = Timer1Timer
    Left = 568
    Top = 328
  end
  object RainTimer: TTimer
    Interval = 5000
    OnTimer = RainTimerTimer
    Left = 568
    Top = 264
  end
end
