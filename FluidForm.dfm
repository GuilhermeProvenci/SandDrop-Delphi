object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Laborat'#243'rio de Fluidos'
  ClientHeight = 600
  ClientWidth = 800
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object PaintBox1: TPaintBox
    Left = 0
    Top = 0
    Width = 600
    Height = 600
    Align = alClient
    OnMouseMove = PaintBox1MouseMove
    OnPaint = PaintBox1Paint
  end
  object pnlControls: TPanel
    Left = 600
    Top = 0
    Width = 200
    Height = 600
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object lblDiff: TLabel
      Left = 10
      Top = 20
      Width = 180
      Height = 15
      Caption = 'Difus'#227'o: 0.10'
    end
    object lblVisc: TLabel
      Left = 10
      Top = 100
      Width = 180
      Height = 15
      Caption = 'Viscosidade: 0.10'
    end
    object tbrDiffusion: TTrackBar
      Left = 10
      Top = 41
      Width = 180
      Height = 45
      Max = 100
      Position = 10
      TabOrder = 0
      OnChange = tbrDiffusionChange
    end
    object tbrViscosity: TTrackBar
      Left = 10
      Top = 121
      Width = 180
      Height = 45
      Max = 100
      Position = 10
      TabOrder = 1
      OnChange = tbrViscosityChange
    end
    object btnReset: TButton
      Left = 10
      Top = 180
      Width = 180
      Height = 35
      Caption = 'Limpar Matrix'
      TabOrder = 2
      OnClick = btnResetClick
    end
    object btnPulse: TButton
      Left = 10
      Top = 225
      Width = 180
      Height = 35
      Caption = 'Gerar Impulso Central'
      TabOrder = 3
      OnClick = btnPulseClick
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 520
    Top = 520
  end
end
