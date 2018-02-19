object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Form3'
  ClientHeight = 356
  ClientWidth = 582
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pgc1: TPageControl
    Left = 0
    Top = 0
    Width = 582
    Height = 356
    ActivePage = tsSettings
    Align = alClient
    TabOrder = 0
    object tsSettings: TTabSheet
      Caption = 'Settings'
      object grpToken: TGroupBox
        Left = 0
        Top = 0
        Width = 574
        Height = 38
        Align = alTop
        Caption = 'Token'
        TabOrder = 0
        object btn1: TButton
          Left = 497
          Top = 15
          Width = 75
          Height = 21
          Align = alRight
          Caption = 'Apply'
          TabOrder = 1
          OnClick = btn1Click
        end
        object edtToken: TEdit
          Left = 2
          Top = 15
          Width = 495
          Height = 21
          Align = alClient
          TabOrder = 0
        end
      end
      object grpChatID: TGroupBox
        Left = 0
        Top = 38
        Width = 574
        Height = 38
        Align = alTop
        Caption = 'Chat ID'
        TabOrder = 1
        object edtChatID: TEdit
          Left = 2
          Top = 15
          Width = 570
          Height = 21
          Align = alClient
          TabOrder = 0
        end
      end
      object mmoInfo: TMemo
        Left = 0
        Top = 76
        Width = 574
        Height = 252
        Align = alClient
        Lines.Strings = (
          '1. '#1042#1089#1090#1072#1074#1080#1090#1100' '#1090#1086#1082#1077#1085' '#1080' '#1085#1072#1078#1072#1090#1100' Apply'
          
            '2. '#1059#1082#1072#1079#1072#1090#1100' Chat ID. '#1045#1089#1083#1080' '#1086#1085' '#1085#1077#1080#1079#1074#1077#1089#1090#1077#1085' - '#1085#1072#1087#1080#1096#1080#1090#1077' '#1083#1102#1073#1086#1077' '#1089#1086#1086#1073#1097#1077#1085#1080 +
            #1077' '#1073#1086#1090#1091' '#1080' Chat ID '#1079#1072#1087#1086#1083#1085#1080#1090#1089#1103' '
          #1072#1074#1090#1086#1084#1072#1090#1080#1095#1077#1089#1082#1080
          '3. '#1057#1084#1086#1090#1088#1077#1090#1100' '#1082#1072#1082' '#1088#1072#1073#1086#1090#1072#1102#1090' '#1084#1077#1090#1086#1076#1099' '#1085#1072' '#1074#1082#1083#1076#1082#1072#1093' '#1089' '#1085#1072#1079#1074#1085#1080#1077#1084' "Send*"'
          ''
          
            #1042#1082#1083#1072#1076#1082#1072' Exceptions '#1089#1086#1076#1077#1088#1078#1080#1090' '#1074' '#1089#1077#1073#1077' '#1089#1087#1080#1089#1086#1082' '#1086#1096#1080#1073#1086#1082' '#1074' '#1087#1088#1086#1094#1077#1089#1089#1077' '#1088#1072#1073#1086 +
            #1090#1099'. '#1045#1089#1083#1080' '#1090#1072#1082#1080#1077' '#1087#1086#1103#1074#1103#1090#1089#1103' - '#1074#1082#1083#1072#1076#1082#1072' '
          #1072#1082#1090#1080#1074#1080#1088#1091#1077#1090#1089#1103' '#1072#1074#1090#1086#1084#1072#1090#1080#1095#1077#1089#1082#1080)
        TabOrder = 2
      end
    end
    object tsExceptions: TTabSheet
      Caption = 'Exceptions'
      ImageIndex = 2
      object mmoExceptions: TMemo
        Left = 0
        Top = 0
        Width = 574
        Height = 328
        Align = alClient
        TabOrder = 0
      end
    end
    object tsSendMessage: TTabSheet
      Caption = 'SendMessage'
      ImageIndex = 1
      object grpSendMsgText: TGroupBox
        Left = 0
        Top = 0
        Width = 574
        Height = 41
        Align = alTop
        Caption = 'grpSendMsgText'
        TabOrder = 0
        object edtSendMsgText: TEdit
          Left = 2
          Top = 15
          Width = 570
          Height = 21
          Align = alTop
          TabOrder = 0
          Text = 'Text Msg'
        end
      end
      object btnSendMsgText: TButton
        Left = 456
        Top = 304
        Width = 115
        Height = 25
        Caption = 'Send'
        TabOrder = 2
        OnClick = btnSendMsgTextClick
      end
      object rgSendMsgParseMode: TRadioGroup
        Left = 0
        Top = 41
        Width = 574
        Height = 40
        Align = alTop
        Caption = 'Parse Mode'
        Columns = 3
        ItemIndex = 0
        Items.Strings = (
          'Default'
          'Markdown'
          'Html')
        TabOrder = 1
      end
      object chkSendMsgDisableNotification: TCheckBox
        Left = 0
        Top = 98
        Width = 574
        Height = 17
        Align = alTop
        Caption = 'Disable Notification'
        TabOrder = 3
      end
      object grpSendMsgReplyToMsgID: TGroupBox
        Left = 0
        Top = 115
        Width = 574
        Height = 41
        Align = alTop
        Caption = 'Reply To MessageId'
        TabOrder = 4
        object seSendMsgReplyToMsgID: TSpinEdit
          Left = 147
          Top = 16
          Width = 150
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 0
          Value = 0
        end
      end
      object chkSendMsgDisableWebPagePreview: TCheckBox
        Left = 0
        Top = 81
        Width = 574
        Height = 17
        Align = alTop
        Caption = 'Disable Web Page Preview'
        TabOrder = 5
      end
    end
    object tsSendPhoto: TTabSheet
      Caption = 'SendPhoto'
      ImageIndex = 3
      object btnSendPhoto: TButton
        Left = 456
        Top = 303
        Width = 115
        Height = 25
        Caption = 'Send'
        TabOrder = 0
        OnClick = btnSendPhotoClick
      end
      object chkSendPhotoNotification: TCheckBox
        Left = 0
        Top = 82
        Width = 574
        Height = 17
        Align = alTop
        Caption = 'Disable Notification'
        TabOrder = 1
      end
      object grpSendPhotoReplyToMsgID: TGroupBox
        Left = 0
        Top = 99
        Width = 574
        Height = 41
        Align = alTop
        Caption = 'Reply To MessageId'
        TabOrder = 2
        object seSendPhotoReplyToMsgID: TSpinEdit
          Left = 147
          Top = 16
          Width = 150
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 0
          Value = 0
        end
      end
      object grpSendPhotoFile: TGroupBox
        Left = 0
        Top = 0
        Width = 574
        Height = 41
        Align = alTop
        Caption = 'Photo'
        TabOrder = 3
        object edtSendPhotoFile: TEdit
          Left = 2
          Top = 15
          Width = 495
          Height = 24
          Align = alClient
          TabOrder = 0
        end
        object btnSendPhotoFile: TButton
          Left = 497
          Top = 15
          Width = 75
          Height = 24
          Align = alRight
          Caption = 'Browse'
          TabOrder = 1
          OnClick = btnSendPhotoFileClick
        end
      end
      object grpSendPhotoCaption: TGroupBox
        Left = 0
        Top = 41
        Width = 574
        Height = 41
        Align = alTop
        Caption = 'Caption'
        TabOrder = 4
        object edtSendPhotoCaption: TEdit
          Left = 2
          Top = 15
          Width = 570
          Height = 21
          Align = alTop
          TabOrder = 0
          Text = 'Photo sign'
        end
      end
    end
    object tsOnUpdates: TTabSheet
      Caption = 'tsOnUpdates'
      ImageIndex = 4
      object mmoOnUpdadtes: TMemo
        Left = 0
        Top = 0
        Width = 574
        Height = 328
        Align = alClient
        Lines.Strings = (
          'mmoOnUpdadtes')
        TabOrder = 0
      end
    end
  end
  object TelegramBot1: TTelegramBot
    ExceptionManager = tgExceptionManagerUI1
    OnReceiveRawData = TelegramBot1ReceiveRawData
    Left = 252
    Top = 304
  end
  object tgReceiverUI1: TtgReceiverUI
    Bot = TelegramBot1
    OnMessage = tgReceiverUI1Message
    Left = 188
    Top = 200
  end
  object tgExceptionManagerUI1: TtgExceptionManagerUI
    OnApiException = tgExceptionManagerUI1ApiException
    OnGlobalException = tgExceptionManagerUI1GlobalException
    Left = 108
    Top = 304
  end
  object OpenDialog1: TOpenDialog
    Left = 260
    Top = 72
  end
end
