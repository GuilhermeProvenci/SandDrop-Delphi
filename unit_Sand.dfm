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
    Left = 758
    Top = 0
    Width = 200
    Height = 709
    Align = alRight
    BevelOuter = bvNone
    Color = 3355443
    ParentBackground = False
    TabOrder = 0
    object pnlHeader: TPanel
      Left = 0
      Top = 0
      Width = 200
      Height = 60
      Align = alTop
      BevelOuter = bvNone
      Color = 2105376
      ParentBackground = False
      TabOrder = 0
      object lblTitle: TLabel
        Left = 0
        Top = 0
        Width = 200
        Height = 60
        Align = alClient
        Alignment = taCenter
        AutoSize = False
        Caption = 'SAND DROP'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -21
        Font.Name = 'Segoe UI Semibold'
        Font.Style = []
        ParentFont = False
        Layout = tlCenter
        ExplicitWidth = 117
        ExplicitHeight = 30
      end
    end
    object pnlBrushGroup: TPanel
      Left = 0
      Top = 60
      Width = 200
      Height = 120
      Align = alTop
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 1
      object lblBrushTitle: TLabel
        Left = 10
        Top = 10
        Width = 180
        Height = 21
        AutoSize = False
        Caption = 'PINCEL'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 12632256
        Font.Height = -13
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object chkEraser: TCheckBox
        Left = 10
        Top = 37
        Width = 180
        Height = 25
        Caption = 'Modo Borracha'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      object tbrRadius: TTrackBar
        Left = 5
        Top = 68
        Width = 190
        Height = 45
        Hint = 'Raio do Pincel'
        Max = 50
        Position = 5
        TabOrder = 1
      end
    end
    object pnlActionGroup: TPanel
      Left = 0
      Top = 180
      Width = 200
      Height = 150
      Align = alTop
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 2
      object lblActionTitle: TLabel
        Left = 10
        Top = 10
        Width = 180
        Height = 21
        AutoSize = False
        Caption = 'A'#199#213'ES'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 12632256
        Font.Height = -13
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object btnReset: TButton
        Left = 10
        Top = 37
        Width = 180
        Height = 35
        Caption = 'Limpar Tudo'
        TabOrder = 0
        OnClick = btnResetClick
      end
      object btnRain: TButton
        Left = 10
        Top = 82
        Width = 180
        Height = 35
        Caption = 'Chuva de Areia'
        TabOrder = 1
        OnClick = btnRainClick
      end
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
