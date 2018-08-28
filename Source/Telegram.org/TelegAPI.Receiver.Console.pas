{***************************************************************************}
{                                                                           }
{           CloudApi for Delphi                                             }
{                                                                           }
{           Copyright (c) 2014-2018 Maxim Sysoev                            }
{                                                                           }
{           https://t.me/CloudAPI                                           }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit TelegAPI.Receiver.Console;

interface

uses
  System.SysUtils,
  TelegAPI.Receiver.Base,
  TelegAPI.Types;

type
  TtgReceiverConsole = class(TTgBotReceiverBase)
  private
    FOnStart: TProc;
    FOnStop: TProc;
    FOnUpdates: TProc<TArray<ItgUpdate>>;
    FOnUpdate: TProc<ItgUpdate>;
    FOnMessage: TProc<ITgMessage>;
    FOnInlineQuery: TProc<ItgInlineQuery>;
    FOnChosenInlineResult: TProc<ItgChosenInlineResult>;
    FOnEditedMessage: TProc<ITgMessage>;
    FOnChannelPost: TProc<ITgMessage>;
    FOnEditedChannelPost: TProc<ITgMessage>;
    FOnShippingQuery: TProc<ItgShippingQuery>;
    FOnPreCheckoutQuery: TProc<ItgPreCheckoutQuery>;
    FOnOnCallbackQuery: TProc<ItgCallbackQuery>;
  protected
    procedure DoOnStart; override;
    procedure DoOnStop; override;
    procedure DoOnUpdates(AUpdates: TArray<ItgUpdate>); override;
    procedure DoOnUpdate(AUpdate: ItgUpdate); override;
    procedure DoOnMessage(AMessage: ITgMessage); override;
    procedure DoOnInlineQuery(AInlineQuery: ItgInlineQuery); override;
    procedure DoOnChosenInlineResult(AChosenInlineResult: ItgChosenInlineResult); override;
    procedure DoOnEditedMessage(AEditedMessage: ITgMessage); override;
    procedure DoOnChannelPost(AChannelPost: ITgMessage); override;
    procedure DoOnEditedChannelPost(AEditedChannelPost: ITgMessage); override;
    procedure DoOnShippingQuery(AShippingQuery: ItgShippingQuery); override;
    procedure DoOnPreCheckoutQuery(APreCheckoutQuery: ItgPreCheckoutQuery); override;
    procedure DoOnCallbackQuery(ACallbackQuery: ItgCallbackQuery); override;
  public
    property OnStart: TProc read FOnStart write FOnStart;
    property OnStop: TProc read FOnStop write FOnStop;
    property OnUpdates: TProc<TArray<ItgUpdate>> read FOnUpdates write FOnUpdates;
    property OnUpdate: TProc<ItgUpdate> read FOnUpdate write FOnUpdate;
    property OnMessage: TProc<ITgMessage> read FOnMessage write FOnMessage;
    property OnInlineQuery: TProc<ItgInlineQuery> read FOnInlineQuery write FOnInlineQuery;
    property OnChosenInlineResult: TProc<ItgChosenInlineResult> read FOnChosenInlineResult write FOnChosenInlineResult;
    property OnEditedMessage: TProc<ITgMessage> read FOnEditedMessage write FOnEditedMessage;
    property OnChannelPost: TProc<ITgMessage> read FOnChannelPost write FOnChannelPost;
    property OnEditedChannelPost: TProc<ITgMessage> read FOnEditedChannelPost write FOnEditedChannelPost;
    property OnShippingQuery: TProc<ItgShippingQuery> read FOnShippingQuery write FOnShippingQuery;
    property OnPreCheckoutQuery: TProc<ItgPreCheckoutQuery> read FOnPreCheckoutQuery write FOnPreCheckoutQuery;
    property OnCallbackQuery: TProc<ItgCallbackQuery> read FOnOnCallbackQuery write FOnOnCallbackQuery;
  end;

implementation

{ TtgRecesiverConsole }

procedure TtgReceiverConsole.DoOnCallbackQuery(ACallbackQuery: ItgCallbackQuery);
begin
  inherited;
  if Assigned(OnCallbackQuery) then
    OnCallbackQuery(ACallbackQuery);
end;

procedure TtgReceiverConsole.DoOnChannelPost(AChannelPost: ITgMessage);
begin
  inherited;
  if Assigned(OnChannelPost) then
    OnChannelPost(AChannelPost);
end;

procedure TtgReceiverConsole.DoOnChosenInlineResult(AChosenInlineResult: ItgChosenInlineResult);
begin
  inherited;
  if Assigned(OnChosenInlineResult) then
    OnChosenInlineResult(AChosenInlineResult);
end;

procedure TtgReceiverConsole.DoOnEditedChannelPost(AEditedChannelPost: ITgMessage);
begin
  inherited;
  if Assigned(OnEditedChannelPost) then
    OnEditedChannelPost(AEditedChannelPost);
end;

procedure TtgReceiverConsole.DoOnEditedMessage(AEditedMessage: ITgMessage);
begin
  inherited;
  if Assigned(OnEditedMessage) then
    OnEditedMessage(AEditedMessage);
end;

procedure TtgReceiverConsole.DoOnInlineQuery(AInlineQuery: ItgInlineQuery);
begin
  inherited;
  if Assigned(OnInlineQuery) then
    OnInlineQuery(AInlineQuery);
end;

procedure TtgReceiverConsole.DoOnMessage(AMessage: ITgMessage);
begin
  inherited;
  if Assigned(OnMessage) then
    OnMessage(AMessage);
end;

procedure TtgReceiverConsole.DoOnPreCheckoutQuery(APreCheckoutQuery: ItgPreCheckoutQuery);
begin
  inherited;
  if Assigned(OnPreCheckoutQuery) then
    OnPreCheckoutQuery(APreCheckoutQuery);
end;

procedure TtgReceiverConsole.DoOnShippingQuery(AShippingQuery: ItgShippingQuery);
begin
  inherited;
  if Assigned(OnShippingQuery) then
    OnShippingQuery(AShippingQuery);
end;

procedure TtgReceiverConsole.DoOnStart;
begin
  inherited;
  if Assigned(OnStart) then
    OnStart();
end;

procedure TtgReceiverConsole.DoOnStop;
begin
  inherited;
  if Assigned(OnStop) then
    OnStop();
end;

procedure TtgReceiverConsole.DoOnUpdate(AUpdate: ItgUpdate);
begin
  inherited;
  if Assigned(OnUpdate) then
    OnUpdate(AUpdate);
end;

procedure TtgReceiverConsole.DoOnUpdates(AUpdates: TArray<ItgUpdate>);
begin
  inherited;
  if Assigned(OnUpdates) then
    OnUpdates(AUpdates);
end;

end.

