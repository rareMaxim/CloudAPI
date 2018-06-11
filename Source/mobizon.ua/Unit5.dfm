object Form5: TForm5
  Left = 0
  Top = 0
  Caption = 'SMS Sender'
  ClientHeight = 280
  ClientWidth = 543
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object grp1: TGroupBox
    Left = 0
    Top = 0
    Width = 543
    Height = 49
    Align = alTop
    Caption = #1053#1086#1084#1077#1088' '#1090#1077#1083#1077#1092#1086#1085#1072
    TabOrder = 0
    object edt1: TEdit
      Left = 2
      Top = 15
      Width = 539
      Height = 21
      Align = alTop
      TabOrder = 0
    end
  end
  object grp2: TGroupBox
    Left = 0
    Top = 49
    Width = 543
    Height = 48
    Align = alTop
    Caption = #1054#1090#1076#1077#1083
    TabOrder = 1
    object cbb1: TComboBox
      Left = 2
      Top = 15
      Width = 539
      Height = 21
      Align = alTop
      ItemIndex = 0
      TabOrder = 0
      Text = #1054#1073#1097#1080#1081
      Items.Strings = (
        #1054#1073#1097#1080#1081
        #1055#1072#1089#1087#1086#1088#1090)
    end
  end
  object grp3: TGroupBox
    Left = 0
    Top = 97
    Width = 543
    Height = 135
    Align = alClient
    Caption = 'grp3'
    TabOrder = 2
    object mmo1: TMemo
      Left = 2
      Top = 15
      Width = 539
      Height = 118
      Align = alClient
      Lines.Strings = (
        #1042#1072#1096#1080' '#1076#1086#1082#1091#1084#1077#1085#1090#1099' '#1075#1086#1090#1086#1074#1099'.')
      MaxLength = 60
      TabOrder = 0
    end
  end
  object btn1: TButton
    Left = 0
    Top = 232
    Width = 543
    Height = 48
    Align = alBottom
    Caption = #1054#1090#1087#1088#1072#1074#1080#1090#1100' '#1089#1086#1086#1073#1097#1077#1085#1080#1077
    Style = bsCommandLink
    TabOrder = 3
    OnClick = btn1Click
  end
  object tmr1: TTimer
    Interval = 500
    OnTimer = tmr1Timer
    Left = 464
    Top = 184
  end
end
