object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'TelegaPi bot test'
  ClientHeight = 584
  ClientWidth = 868
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 182
    Width = 868
    Height = 402
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object btnStart: TButton
    Left = 111
    Top = 15
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 1
    OnClick = btnStartClick
  end
  object btnStop: TButton
    Left = 192
    Top = 15
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 2
    OnClick = btnStopClick
  end
  object spinInterval: TSpinEdit
    Left = 8
    Top = 18
    Width = 97
    Height = 22
    Hint = #1048#1085#1090#1077#1088#1074#1072#1083' '#1087#1088#1086#1074#1077#1088#1082#1080' '#1089#1086#1086#1073#1097#1077#1085#1080#1081' '#1085#1072' '#1089#1077#1088#1074#1077#1088#1077
    Increment = 1000
    MaxValue = 0
    MinValue = 0
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    Value = 1000
    OnChange = spinIntervalChange
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 46
    Width = 417
    Height = 123
    Caption = #1057#1086#1086#1073#1097#1077#1085#1080#1077' '#1074' '#1082#1072#1085#1072#1083
    TabOrder = 4
    object edChatId: TEdit
      Left = 3
      Top = 24
      Width = 154
      Height = 21
      TabOrder = 0
      TextHint = 'chat_id'
    end
    object edChatUserName: TEdit
      Left = 3
      Top = 51
      Width = 154
      Height = 21
      TabOrder = 1
      TextHint = '@UserChannelName'
    end
    object edChatMsgText: TEdit
      Left = 176
      Top = 24
      Width = 217
      Height = 21
      TabOrder = 2
      TextHint = #1042#1074#1077#1076#1080#1090#1077' '#1090#1077#1082#1089#1090' '#1089#1086#1086#1073#1097#1077#1085#1080#1103
    end
    object btnChatSendMsg: TButton
      Left = 264
      Top = 51
      Width = 129
      Height = 25
      Caption = 'Send to channel'
      TabOrder = 3
      OnClick = btnChatSendMsgClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 431
    Top = 34
    Width = 425
    Height = 135
    Caption = #1051#1080#1095#1085#1086#1077' '#1089#1086#1086#1073#1097#1077#1085#1080#1077' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1102
    TabOrder = 5
    object edUserId: TEdit
      Left = 11
      Top = 32
      Width = 154
      Height = 21
      TabOrder = 0
      TextHint = 'user_id'
    end
    object edUserMsgText: TEdit
      Left = 184
      Top = 32
      Width = 217
      Height = 21
      TabOrder = 1
      TextHint = #1042#1074#1077#1076#1080#1090#1077' '#1090#1077#1082#1089#1090' '#1089#1086#1086#1073#1097#1077#1085#1080#1103
    end
    object btnUserSend: TButton
      Left = 256
      Top = 59
      Width = 145
      Height = 25
      Caption = 'Send to user'
      TabOrder = 2
      OnClick = btnUserSendClick
    end
  end
  object TelegramBot1: TTelegramBot
    Domain = 'https://api.telegram.org/bot'
    Token = '630004982:AAFtjqPkJFbY77XACgrV8KY-RT2g3bVfvPg'
    OnReceiveRawData = TelegramBot1ReceiveRawData
    OnSendData = TelegramBot1SendData
    OnError = TelegramBot1Error
    Left = 176
    Top = 232
  end
  object tgReceiverUI1: TtgReceiverUI
    Bot = TelegramBot1
    OnUpdates = tgReceiverUI1Updates
    OnUpdate = tgReceiverUI1Update
    OnMessage = tgReceiverUI1Message
    OnChannelPost = tgReceiverUI1ChannelPost
    Left = 104
    Top = 232
  end
  object UniConnection1: TUniConnection
    ProviderName = 'MySQL'
    Port = 3306
    Database = 'kvartal_ekr'
    SpecificOptions.Strings = (
      'MySQL.Compress=True'
      'MySQL.UseUnicode=True')
    Username = 'kvartal_ekr2'
    Server = 'kvartal.ua'
    LoginPrompt = False
    Left = 288
    Top = 304
    EncryptedPassword = 'B4FF89FF9EFF8DFF8BFF9EFF93FFD2FFCFFFCFFFCFFF'
  end
  object MySQLUniProvider1: TMySQLUniProvider
    Left = 335
    Top = 286
  end
  object qMed: TUniQuery
    Connection = UniConnection1
    SQL.Strings = (
      'select'
      '  p.num_phone, o.name'
      'from'
      '  phones p, object_n o'
      ''
      'where'
      '  o.id = p.id_object_n'
      'AND'
      '  p.is_request=0'
      'and'
      '  o.id_supertype=8'
      'and '
      '  (p.num_phone = :num_phone or p.num_phone = :num_phone2)')
    UniDirectional = True
    Options.AutoPrepare = True
    Left = 383
    Top = 302
    ParamData = <
      item
        DataType = ftUnknown
        Name = 'num_phone'
        Value = nil
      end
      item
        DataType = ftUnknown
        Name = 'num_phone2'
        Value = nil
      end>
  end
end
