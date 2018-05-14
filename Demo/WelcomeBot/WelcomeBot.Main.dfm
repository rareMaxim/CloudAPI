object WbMain: TWbMain
  OldCreateOrder = False
  DisplayName = 'Welcome Bot'
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 457
  Width = 626
  object cuHttpClientSysNet1: TcuHttpClientSysNet
    Left = 176
    Top = 168
  end
  object TelegramBot1: TTelegramBot
    HttpCore = cuHttpClientSysNet1
    ExceptionManager = tgExceptionManagerUI1
    Left = 136
    Top = 104
  end
  object tgExceptionManagerUI1: TtgExceptionManagerUI
    Left = 48
    Top = 168
  end
  object tgReceiverService1: TtgReceiverService
    Bot = TelegramBot1
    OnMessage = tgReceiverService1Message
    Left = 136
    Top = 24
  end
end
