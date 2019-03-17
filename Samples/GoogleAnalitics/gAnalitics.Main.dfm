object Form4: TForm4
  Left = 0
  Top = 0
  Caption = 'Form4'
  ClientHeight = 448
  ClientWidth = 581
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
  object mmo1: TMemo
    Left = 0
    Top = 112
    Width = 581
    Height = 336
    Align = alBottom
    Lines.Strings = (
      'mmo1')
    TabOrder = 0
  end
  object gaAnalitics1: TgaAnalitics
    General.TrackingId = 'UA-77812232-2'
    General.AnonymizingIP = False
    General.QueueTime = 0
    User.ClientID = '1253456789'
    Session.SessionController = Empty
    SystemInfo.JavaEnabled = False
    Hit.HitType = pageview
    AppTracking.ApplicationName = 'CloudAPI Demo'
    AppTracking.ApplicationID = '1234567890'
    AppTracking.ApplicationVersion = '0.1'
    AppTracking.ApplicationInstallerID = '1'
    Exception.IsFatal = False
    OnSendData = gaAnalitics1SendData
    OnDebug = gaAnalitics1Debug
    IsDebug = False
    Left = 384
    Top = 256
  end
  object AppAnalytics1: TAppAnalytics
    Active = False
    CacheSize = 500
    UpdateInterval = 600
    Options = [aoTrackStartup, aoTrackFormActivate, aoTrackExceptions]
    PrivacyMessage.Strings = (
      'Privacy Notice:'
      ''
      
        'This application anonymously tracks your usage and sends it to u' +
        's for analysis. We use this analysis to make the software work b' +
        'etter for you.'
      ''
      
        'This tracking is completely anonymous. No personally identifying' +
        ' information is tracked, and nothing about your usage can be tra' +
        'cked back to you.'
      ''
      'Thank you for helping us to improve this software.')
    Left = 200
    Top = 112
  end
end
