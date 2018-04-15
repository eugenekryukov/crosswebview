object Form51: TForm51
  Left = 0
  Top = 0
  ActiveControl = URLEdit
  Caption = 'CrossVCL Mini Browser'
  ClientHeight = 573
  ClientWidth = 884
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 884
    Height = 47
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      884
      47)
    object URLEdit: TEdit
      Left = 104
      Top = 14
      Width = 685
      Height = 26
      Anchors = [akLeft, akTop, akRight]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      Text = 'https://www.ksdev.com'
    end
    object Go: TButton
      Left = 801
      Top = 14
      Width = 75
      Height = 26
      Anchors = [akTop, akRight]
      Caption = 'Go'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = GoClick
    end
    object BackButton: TButton
      Left = 11
      Top = 14
      Width = 38
      Height = 26
      Caption = '<'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnClick = BackButtonClick
    end
    object ForwButton: TButton
      Left = 55
      Top = 14
      Width = 38
      Height = 26
      Caption = '>'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnClick = ForwButtonClick
    end
  end
  object WebView1: TWebView
    Left = 0
    Top = 47
    Width = 884
    Height = 507
    Align = alClient
    UseDockManager = False
    ParentBackground = False
    OnFinishLoading = WebView1FinishLoading
    ExplicitLeft = 74
    ExplicitTop = 142
    ExplicitWidth = 810
    ExplicitHeight = 412
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 554
    Width = 884
    Height = 19
    Panels = <>
    SimplePanel = True
  end
end
