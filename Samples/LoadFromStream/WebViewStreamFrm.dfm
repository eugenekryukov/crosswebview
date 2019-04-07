object WebViewForm: TWebViewForm
  Left = 0
  Top = 0
  Caption = 'CrossVCL - WebView'
  ClientHeight = 475
  ClientWidth = 795
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 218
    Width = 795
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitWidth = 215
  end
  object Memo1: TMemo
    Left = 0
    Top = 41
    Width = 795
    Height = 177
    Align = alTop
    Lines.Strings = (
      '<!DOCTYPE html>'
      '<html>'
      '<head>'
      '  <title>Hello, World!</title>'
      '</head>'
      ''
      '<body>'
      'HTML Code'
      '</body>'
      '</html>')
    TabOrder = 0
    ExplicitWidth = 651
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 795
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 651
    object Button1: TButton
      Left = 17
      Top = 10
      Width = 153
      Height = 25
      Caption = 'LoadFromString'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 184
      Top = 10
      Width = 137
      Height = 25
      Caption = 'LoadFromStream'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 344
      Top = 10
      Width = 113
      Height = 25
      Caption = 'Get selection'
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 472
      Top = 10
      Width = 113
      Height = 25
      Caption = 'Run JavaScript'
      TabOrder = 3
      OnClick = Button4Click
    end
  end
  object WebView1: TWebView
    Left = 0
    Top = 221
    Width = 795
    Height = 254
    Align = alClient
    UseDockManager = False
    ParentBackground = False
    ExplicitWidth = 651
    ExplicitHeight = 212
  end
end
