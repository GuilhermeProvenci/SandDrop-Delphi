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
  object pnlSidebar: TPanel
    Left = 778
    Top = 0
    Width = 180
    Height = 709
    Align = alRight
    TabOrder = 0
    object chkEraser: TCheckBox
      Left = 1
      Top = 1
      Width = 178
      Height = 25
      Align = alTop
      Caption = 'Borracha'
      TabOrder = 0
    end
    object tbrRadius: TTrackBar
      Left = 1
      Top = 26
      Width = 178
      Height = 45
      Hint = 'Raio do Pincel'
      Align = alTop
      Max = 100
      Position = 5
      TabOrder = 1
    end
    object btnReset: TButton
      Left = 1
      Top = 71
      Width = 178
      Height = 30
      Align = alTop
      Caption = 'Reset'
      TabOrder = 2
      OnClick = btnResetClick
    end
    object btnRain: TButton
      Left = 1
      Top = 101
      Width = 178
      Height = 30
      Align = alTop
      Caption = 'Rain'
      TabOrder = 3
      OnClick = btnRainClick
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 778
    Height = 709
    Align = alClient
    DoubleBufferedMode = dbmRequested
    TabOrder = 1
    object PaintBox1: TPaintBox
      Left = 1
      Top = 1
      Width = 776
      Height = 707
      Align = alClient
      OnMouseMove = PaintBox1MouseMove
      OnPaint = PaintBox1Paint
    end
    object lblFPS: TLabel
      Left = 10
      Top = 10
      Width = 44
      Height = 21
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
