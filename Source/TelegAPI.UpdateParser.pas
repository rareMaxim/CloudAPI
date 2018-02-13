unit TelegAPI.UpdateParser;

interface

uses
  System.Classes,
  System.SysUtils,
  TelegAPI.Base,
  TelegAPI.Bot,
  TelegAPI.Types,
  TelegAPI.Types.Enums;

type
  TTgBotUpdateParser = class(TtgAbstractComponent)
  private
  protected
    procedure EventParser(AUpdates: TArray<ItgUpdate>); virtual;
    procedure TypeUpdate(AUpdate: ItgUpdate); virtual;
    //События
    procedure DoOnUpdates(AUpdates: TArray<ItgUpdate>); virtual; abstract;
    procedure DoOnUpdate(AUpdate: ItgUpdate); virtual; abstract;
    procedure DoOnMessage(AMessage: ITgMessage); virtual; abstract;
    procedure DoOnInlineQuery(AInlineQuery: ItgInlineQuery); virtual; abstract;
    procedure DoOnChosenInlineResult(AChosenInlineResult: ItgChosenInlineResult); virtual; abstract;
    procedure DoOnCallbackQuery(ACallbackQuery: ItgCallbackQuery); virtual; abstract;
    procedure DoOnEditedMessage(AEditedMessage: ITgMessage); virtual; abstract;
    procedure DoOnChannelPost(AChannelPost: ITgMessage); virtual; abstract;
    procedure DoOnEditedChannelPost(AEditedChannelPost: ITgMessage); virtual; abstract;
    procedure DoOnShippingQuery(AShippingQuery: ItgShippingQuery); virtual; abstract;
    procedure DoOnPreCheckoutQuery(APreCheckoutQuery: ItgPreCheckoutQuery); virtual; abstract;
  public
    procedure ParseResponse(const JSON: string);
  end;

implementation

uses
  TelegAPI.Bot.Impl;
{ TTgBotReceiverBase }

procedure TTgBotUpdateParser.EventParser(AUpdates: TArray<ItgUpdate>);
var
  LUpdate: ItgUpdate;
begin
  DoOnUpdates(AUpdates);
  for LUpdate in AUpdates do
  begin
    DoOnUpdate(LUpdate);
    TypeUpdate(LUpdate);
  end;
end;

procedure TTgBotUpdateParser.ParseResponse(const JSON: string);
var
  LUpdates: TArray<ItgUpdate>;
  LBot: ITelegramBot;
begin
  LBot := TTelegramBot.Create(nil);
  LUpdates := LBot.GetUpdates(JSON);
  EventParser(LUpdates);
end;

procedure TTgBotUpdateParser.TypeUpdate(AUpdate: ItgUpdate);
begin
  case AUpdate.&Type of
    TtgUpdateType.MessageUpdate:
      DoOnMessage(AUpdate.Message);

    TtgUpdateType.InlineQueryUpdate:
      DoOnInlineQuery(AUpdate.InlineQuery);

    TtgUpdateType.ChosenInlineResultUpdate:
      DoOnChosenInlineResult(AUpdate.ChosenInlineResult);

    TtgUpdateType.CallbackQueryUpdate:
      DoOnCallbackQuery(AUpdate.CallbackQuery);

    TtgUpdateType.EditedMessage:
      DoOnEditedMessage(AUpdate.EditedMessage);

    TtgUpdateType.ChannelPost:
      DoOnChannelPost(AUpdate.ChannelPost);

    TtgUpdateType.EditedChannelPost:
      DoOnEditedChannelPost(AUpdate.EditedChannelPost);

    TtgUpdateType.ShippingQueryUpdate:
      DoOnShippingQuery(AUpdate.ShippingQuery);

    TtgUpdateType.PreCheckoutQueryUpdate:
      DoOnPreCheckoutQuery(AUpdate.PreCheckoutQuery);
  end;
end;

end.

