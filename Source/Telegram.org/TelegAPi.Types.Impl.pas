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

unit TelegAPi.Types.Impl;

interface

uses
  CloudAPI.Utils.Json,
  System.SysUtils,
  TelegAPi.Types.Enums,
  TelegAPi.Types;

type
  TtgUser = class(TBaseJson, ItgUser)
  public
    function ID: Int64;
    function IsBot: Boolean;
    function FirstName: string;
    function LastName: string;
    function Username: string;
    function LanguageCode: string;
  end;

  TtgChatMember = class(TBaseJson, ItgChatMember)
  public
    function User: ItgUser;
    function Status: TtgChatMemberStatus;
    function UntilDate: TDateTime;
    function CanBeEdited: Boolean;
    function CanChangeInfo: Boolean;
    function CanPostMessages: Boolean;
    function CanEditMessages: Boolean;
    function CanDeleteMessages: Boolean;
    function CanInviteUsers: Boolean;
    function CanRestrictMembers: Boolean;
    function CanPinMessages: Boolean;
    function CanPromoteMembers: Boolean;
    function CanSendMessages: Boolean;
    function CanSendMediaMessages: Boolean;
    function CanSendOtherMessages: Boolean;
    function CanAddWebPagePreviews: Boolean;
  end;

  TtgChatPhoto = class(TBaseJson, ItgChatPhoto)
    function SmallFileId: string;
    function BigFileId: string;
  end;

  TtgChat = class(TBaseJson, ItgChat)
  public
    function ID: Int64;
    function TypeChat: TtgChatType;
    function Title: string;
    function Username: string;
    function FirstName: string;
    function LastName: string;
    function AllMembersAreAdministrators: Boolean;
    function Photo: ItgChatPhoto;
    function Description: string;
    function InviteLink: string;
    function PinnedMessage: ITgMessage;
    function StickerSetName: string;
    function CanSetStickerSet: Boolean;
  end;

  TtgMessageEntity = class(TBaseJson, ItgMessageEntity)
  public
    function TypeMessage: TtgMessageEntityType;
    function Offset: Int64;
    function Length: Int64;
    function Url: string;
    function User: ItgUser;
  end;

  TtgFile = class(TBaseJson, ItgFile)
  public
    function FileId: string;
    function FileSize: Int64;
    function FilePath: string;
    function CanDownload: Boolean;
    function GetFileUrl(const AToken: string): string;
  end;

  TtgAudio = class(TtgFile, ItgAudio)
  public
    function Duration: Int64;
    function Performer: string;
    function Title: string;
    function MimeType: string;
    function Thumb: ItgPhotoSize;
  end;

  TtgPhotoSize = class(TtgFile, ItgPhotoSize)
  public
    function Width: Int64;
    function Height: Int64;
  end;

  TtgDocument = class(TtgFile, ItgDocument)
  public
    function Thumb: ItgPhotoSize;
    function FileName: string;
    function MimeType: string;
  end;

  TtgMaskPosition = class(TBaseJson, ItgMaskPosition)
    function Point: TtgMaskPositionPoint;
    function XShift: Single;
    function YShift: Single;
    function Scale: Single;
  end;

  TtgSticker = class(TtgFile, ItgSticker)
  public
    function Width: Int64;
    function Height: Int64;
    function Thumb: ItgPhotoSize;
    function Emoji: string;
    function SetName: string;
    function MaskPosition: ItgMaskPosition;
  end;

  TtgStickerSet = class(TBaseJson, ItgStickerSet)
  public
    function Name: string;
    function Title: string;
    function ContainsMasks: Boolean;
    function Stickers: TArray<ItgSticker>;
  end;

  TtgVideo = class(TtgFile, ItgVideo)
  public
    function Width: Int64;
    function Height: Int64;
    function Duration: Int64;
    function Thumb: ItgPhotoSize;
    function MimeType: string;
  end;

  TtgVideoNote = class(TtgFile, ItgVideoNote)
  public
    function FileId: string;
    function Length: Int64;
    function Duration: Int64;
    function Thumb: ItgPhotoSize;
    function FileSize: Int64;
  end;

  TtgVoice = class(TtgFile, ItgVoice)
  public
    function Duration: Int64;
    function MimeType: string;
  end;

  TtgContact = class(TBaseJson, ItgContact)
  private
    function GetPhoneNumber: string;
    procedure SetPhoneNumber(const AValue: string);
    function GetFirstName: string;
    procedure SetFirstName(const AValue: string);
    function GetLastName: string;
    procedure SetLastName(const AValue: string);
    function GetUserId: Int64;
    procedure SetUserId(const AValue: Int64);
    function GetvCard: string;
    procedure SetvCard(const Value: string);
  public
    constructor Create(const AFirstName, ALastName, APhoneNumber: string); reintroduce;
    property PhoneNumber: string read GetPhoneNumber write SetPhoneNumber;
    property FirstName: string read GetFirstName write SetFirstName;
    property LastName: string read GetLastName write SetLastName;
    property UserId: Int64 read GetUserId write SetUserId;
    property vCard: string read GetvCard write SetvCard;
  end;

  TtgLocation = class(TBaseJson, ItgLocation)
  private
    function GetLongitude: Double;
    function GetLatitude: Double;
    procedure SetLatitude(const Value: Double);
    procedure SetLongitude(const Value: Double);
  public
    constructor Create(const ALongitude, ALatitude: Double); reintroduce; overload;
    constructor Create(const AJson: string); overload; override;
    property Longitude: Double read GetLongitude write SetLongitude;
    property Latitude: Double read GetLatitude write SetLatitude;
  end;

  TtgVenue = class(TBaseJson, ItgVenue)
  private
    function GetLocation: ItgLocation;
    procedure SetLocation(const AValue: ItgLocation);
    function GetTitle: string;
    procedure SetTitle(const AValue: string);
    function GetAddress: string;
    procedure SetAddress(const AValue: string);
    function GetFoursquareId: string;
    procedure SetFoursquareId(const AValue: string);
    function GetFoursquareType: string;
    procedure SetFoursquareType(const Value: string);
  public
    property Location: ItgLocation read GetLocation write SetLocation;
    property Title: string read GetTitle write SetTitle;
    property Address: string read GetAddress write SetAddress;
    property FoursquareId: string read GetFoursquareId write SetFoursquareId;
    property FoursquareType: string read GetFoursquareType write SetFoursquareType;
  end;

  TtgAnimation = class(TtgVideo, ItgAnimation)
  public
    function FileName: string;
  end;

  TtgGameHighScore = class(TBaseJson, ItgGameHighScore)
  public
    function Position: Int64;
    function User: ItgUser;
    function Score: Int64;
  end;

  TtgGame = class(TBaseJson, ItgGame)
  public
    function Title: string;
    function Description: string;
    function Photo: TArray<ItgPhotoSize>;
    function Text: string;
    function TextEntities: TArray<ItgMessageEntity>;
    function Animation: ItgAnimation;
  end;

  TTgMessage = class(TBaseJson, ITgMessage)
  public
    function MessageId: Int64;
    function From: ItgUser;
    function Date: TDateTime;
    function Chat: ItgChat;
    function ForwardFrom: ItgUser;
    function ForwardFromChat: ItgChat;
    function ForwardFromMessageId: Int64;
    function ForwardSignature: string;
    function ForwardDate: TDateTime;
    function ReplyToMessage: ITgMessage;
    function EditDate: TDateTime;
    function AuthorSignature: string;
    function Text: string;
    function Entities: TArray<ItgMessageEntity>;
    function CaptionEntities: TArray<ItgMessageEntity>;
    function Audio: ItgAudio;
    function Document: ItgDocument;
    function Animation: ItgAnimation;
    function Game: ItgGame;
    function Photo: TArray<ItgPhotoSize>;
    function Sticker: ItgSticker;
    function Video: ItgVideo;
    function Voice: ItgVoice;
    function VideoNote: ItgVideoNote;
    function NewChatMembers: TArray<ItgUser>;
    function Caption: string;
    function Contact: ItgContact;
    function Location: ItgLocation;
    function Venue: ItgVenue;
    function NewChatMember: ItgUser;
    function LeftChatMember: ItgUser;
    function NewChatTitle: string;
    function NewChatPhoto: TArray<ItgPhotoSize>;
    function DeleteChatPhoto: Boolean;
    function GroupChatCreated: Boolean;
    function SupergroupChatCreated: Boolean;
    function ChannelChatCreated: Boolean;
    function MigrateToChatId: Int64;
    function MigrateFromChatId: Int64;
    function PinnedMessage: ITgMessage;
    function Invoice: ItgInvoice;
    function SuccessfulPayment: ItgSuccessfulPayment;
    function ConnectedWebsite: string;
    function &Type: TtgMessageType;
    function IsCommand(const AValue: string): Boolean;
  end;

  TtgUserProfilePhotos = class(TBaseJson, ItgUserProfilePhotos)
  public
    function TotalCount: Int64;
    function Photos: TArray<TArray<ItgPhotoSize>>;
  end;

  TtgCallbackGame = class
  end;

  TtgResponseParameters = class(TBaseJson, ItgResponseParameters)
  public
    function MigrateToChatId: Int64;
    function RetryAfter: Int64;
  end;

  TtgInlineQuery = class(TBaseJson, ItgInlineQuery)
  public
    function ID: string;
    function From: ItgUser;
    function Query: string;
    function Offset: string;
  end;

  TtgChosenInlineResult = class(TBaseJson, ItgChosenInlineResult)
  public
    function ResultId: string;
    function From: ItgUser;
    function Location: ItgLocation;
    function InlineMessageId: string;
    function Query: string;
  end;

  TtgCallbackQuery = class(TBaseJson, ItgCallbackQuery)
  public
    function ID: string;
    function From: ItgUser;
    function Message: ITgMessage;
    function InlineMessageId: string;
    function Data: string;
    function GameShortName: string;
  end;

{$REGION 'Payments'}

  TtgInvoice = class(TBaseJson, ItgInvoice)
  public
    function Title: string;
    function Description: string;
    function StartParameter: string;
    function Currency: string;
    function TotalAmount: Int64;
  end;

  TtgLabeledPrice = class(TBaseJson, ItgLabeledPrice)
  public
    function Text: string;
    function Amount: Int64;
    // constructor Create(const AText: string; AAmount: Int64); overload;
  end;

  /// <summary>
  /// This object represents a shipping address.
  /// </summary>
  TtgShippingAddress = class(TBaseJson, ItgShippingAddress)
  public
    function CountryCode: string;
    function State: string;
    function City: string;
    function StreetLine1: string;
    function StreetLine2: string;
    function PostCode: string;
  end;

  TtgOrderInfo = class(TBaseJson, ItgOrderInfo)
  public
    function Name: string;
    function PhoneNumber: string;
    function Email: string;
    function ShippingAddress: ItgShippingAddress;
  end;

  TtgPreCheckoutQuery = class(TBaseJson, ItgPreCheckoutQuery)
  public
    function ID: string;
    function From: ItgUser;
    function Currency: string;
    function TotalAmount: Int64;
    function InvoicePayload: string;
    function ShippingOptionId: string;
    function OrderInfo: ItgOrderInfo;
  end;

  TtgShippingOption = class(TBaseJson, ItgShippingOption)
  public
    function ID: string;
    function Title: string;
    function Prices: TArray<ItgLabeledPrice>;
  end;

  TtgShippingQuery = class(TBaseJson, ItgShippingQuery)
  public
    function ID: string;
    function From: ItgUser;
    function InvoicePayload: string;
    function ShippingAddress: ItgShippingAddress;
  end;

  TtgSuccessfulPayment = class(TBaseJson, ItgSuccessfulPayment)
  public
    function Currency: string;
    function TotalAmount: Int64;
    function InvoicePayload: string;
    function ShippingOptionId: string;
    function OrderInfo: ItgOrderInfo;
    function TelegramPaymentChargeId: string;
    function ProviderPaymentChargeId: string;
  end;
{$ENDREGION}

  TtgUpdate = class(TBaseJson, ItgUpdate)
  public
    function ID: Int64;
    function Message: ITgMessage;
    function EditedMessage: ITgMessage;
    function InlineQuery: ItgInlineQuery;
    function ChosenInlineResult: ItgChosenInlineResult;
    function CallbackQuery: ItgCallbackQuery;
    function ChannelPost: ITgMessage;
    function EditedChannelPost: ITgMessage;
    function ShippingQuery: ItgShippingQuery;
    function PreCheckoutQuery: ItgPreCheckoutQuery;
    function &Type: TtgUpdateType;
  end;

  TtgWebhookInfo = class(TBaseJson, ItgWebhookInfo)
  public
    function Url: string;
    function HasCustomCertificate: Boolean;
    function PendingUpdateCount: Int64;
    function LastErrorDate: TDateTime;
    function LastErrorMessage: string;
    function MaxConnections: Int64;
    function AllowedUpdates: TArray<string>;
  end;

implementation

uses
  System.Json,
  System.TypInfo,
  System.Generics.Collections;

{ TtgAnimation }

function TtgAnimation.FileName: string;
begin
  Result := ToSimpleType<string>('file_name');
end;

{ TtgCallbackQuery }
function TtgCallbackQuery.Data: string;
begin
  Result := ToSimpleType<string>('data');
end;

function TtgCallbackQuery.From: ItgUser;
begin
  Result := ToClass<TtgUser>('from');
end;

function TtgCallbackQuery.GameShortName: string;
begin
  Result := ToSimpleType<string>('game_short_name');
end;

function TtgCallbackQuery.ID: string;
begin
  Result := ToSimpleType<string>('id');
end;

function TtgCallbackQuery.InlineMessageId: string;
begin
  Result := ToSimpleType<string>('inline_message_id');
end;

function TtgCallbackQuery.Message: ITgMessage;
begin
  Result := ToClass<TTgMessage>('message');
end;

{ TtgDocument }

function TtgDocument.FileName: string;
begin
  Result := ToSimpleType<string>('file_name');
end;

function TtgDocument.MimeType: string;
begin
  Result := ToSimpleType<string>('mime_type');
end;

function TtgDocument.Thumb: ItgPhotoSize;
begin
  Result := ToClass<TtgPhotoSize>('thumb');
end;

{ TtgFile }
function TtgFile.CanDownload: Boolean;
begin
  Result := not FilePath.IsEmpty;
end;

function TtgFile.FileId: string;
begin
  Result := ToSimpleType<string>('file_id');
end;

function TtgFile.FilePath: string;
begin
  Result := ToSimpleType<string>('file_path');
end;

function TtgFile.FileSize: Int64;
begin
  Result := ToSimpleType<Int64>('file_size');
end;

function TtgFile.GetFileUrl(const AToken: string): string;
begin
  Result := 'https://api.telegram.org/file/bot' + AToken + '/' + FilePath;
end;

function TtgGameHighScore.Position: Int64;
begin
  Result := ToSimpleType<Int64>('position');
end;

function TtgGameHighScore.Score: Int64;
begin
  Result := ToSimpleType<Int64>('score');
end;

function TtgGameHighScore.User: ItgUser;
begin
  Result := ToClass<TtgUser>('user');
end;

{ TtgMessage }

function TTgMessage.Document: ItgDocument;
begin
  Result := ToClass<TtgDocument>('document');
end;

function TTgMessage.EditDate: TDateTime;
begin
  Result := ToDateTime('edit_date');
end;

function TTgMessage.ForwardDate: TDateTime;
begin
  Result := ToDateTime('forward_date');
end;

function TTgMessage.ForwardFrom: ItgUser;
begin
  Result := ToClass<TtgUser>('forward_from');
end;

function TTgMessage.ForwardFromChat: ItgChat;
begin
  Result := ToClass<TtgChat>('forward_from_chat');
end;

function TTgMessage.ForwardFromMessageId: Int64;
begin
  Result := ToSimpleType<Int64>('forward_from_message_id');
end;

function TTgMessage.ForwardSignature: string;
begin
  Result := ToSimpleType<string>('forward_signature');
end;

function TTgMessage.From: ItgUser;
begin
  Result := ToClass<TtgUser>('from');
end;

function TTgMessage.Game: ItgGame;
begin
  Result := ToClass<TtgGame>('game');
end;

function TTgMessage.GroupChatCreated: Boolean;
begin
  Result := ToSimpleType<Boolean>('group_chat_created');
end;

function TTgMessage.Invoice: ItgInvoice;
begin
  Result := ToClass<TtgInvoice>('invoice');
end;

function TTgMessage.IsCommand(const AValue: string): Boolean;
var
  LEnt: ItgMessageEntity;
begin
  Result := False;
  if Self.Entities = nil then
    Exit;
  for LEnt in Self.Entities do
    if (LEnt.TypeMessage = TtgMessageEntityType.BotCommand) then
      if Text.Substring(LEnt.Offset, LEnt.Length).StartsWith(AValue, True) then
        Exit(True);
end;

function TTgMessage.LeftChatMember: ItgUser;
begin
  Result := ToClass<TtgUser>('left_chat_member');
end;

function TTgMessage.Location: ItgLocation;
begin
  Result := ToClass<TtgLocation>('location');
end;

function TTgMessage.MessageId: Int64;
begin
  Result := ToSimpleType<Int64>('message_id');
end;

function TTgMessage.MigrateFromChatId: Int64;
begin
  Result := ToSimpleType<Int64>('migrate_from_chat_id');
end;

function TTgMessage.MigrateToChatId: Int64;
begin
  Result := ToSimpleType<Int64>('migrate_to_chat_id');
end;

function TTgMessage.NewChatMember: ItgUser;
begin
  Result := ToClass<TtgUser>('new_chat_member');
end;

function TTgMessage.NewChatMembers: TArray<ItgUser>;
var
  LValue: string;
  LJsonArray: TJSONArray;
  I: Integer;
begin
  Result := nil;
  if GetJson.TryGetValue<string>('new_chat_members', LValue) then
  begin
    LJsonArray := TJSONObject.ParseJSONValue(LValue) as TJSONArray;
    try
      SetLength(Result, LJsonArray.Count);
      for I := 0 to LJsonArray.Count - 1 do
        Result[I] := ToClass<TtgUser>('new_chat_members');
    finally
      LJsonArray.Free;
    end;
  end;
end;

function TTgMessage.NewChatPhoto: TArray<ItgPhotoSize>;
var
  LValue: string;
  LJsonArray: TJSONArray;
  I: Integer;
begin
  Result := nil;
  if GetJson.TryGetValue<string>('new_chat_photo', LValue) then
  begin
    LJsonArray := TJSONObject.ParseJSONValue(LValue) as TJSONArray;
    try
      SetLength(Result, LJsonArray.Count);
      for I := 0 to LJsonArray.Count - 1 do
        Result[I] := ToClass<TtgPhotoSize>('new_chat_photo');
    finally
      LJsonArray.Free;
    end;
  end;
end;

function TTgMessage.NewChatTitle: string;
begin
  Result := ToSimpleType<string>('new_chat_title');
end;

function TTgMessage.Entities: TArray<ItgMessageEntity>;
var
  LJsonArray: TJSONArray;
  I: Integer;
begin
  LJsonArray := GetJson.GetValue('entities') as TJSONArray;
  if (not Assigned(LJsonArray)) or LJsonArray.Null then
    Exit(nil);
  SetLength(Result, LJsonArray.Count);
  for I := 0 to LJsonArray.Count - 1 do
    Result[I] := TtgMessageEntity.Create(LJsonArray.Items[I].ToString);
end;

function TTgMessage.Photo: TArray<ItgPhotoSize>;
begin
  Result := ToArray<ItgPhotoSize>(TtgPhotoSize, 'photo');
end;

function TTgMessage.PinnedMessage: ITgMessage;
begin
  Result := ToClass<TTgMessage>('pinned_message');
end;

function TTgMessage.ReplyToMessage: ITgMessage;
begin
  Result := ToClass<TTgMessage>('reply_to_message');
end;

function TTgMessage.Sticker: ItgSticker;
begin
  Result := ToClass<TtgSticker>('sticker');
end;

function TTgMessage.SuccessfulPayment: ItgSuccessfulPayment;
begin
  Result := ToClass<TtgSuccessfulPayment>('successful_payment');
end;

function TTgMessage.SupergroupChatCreated: Boolean;
begin
  Result := ToSimpleType<Boolean>('supergroup_chat_created');
end;

function TTgMessage.Text: string;
begin
  Result := ToSimpleType<string>('text');
end;

function TTgMessage.&Type: TtgMessageType;
begin
  if Audio <> nil then
    Exit(TtgMessageType.Audio);
  if Contact <> nil then
    Exit(TtgMessageType.Contact);
  if Document <> nil then
    Exit(TtgMessageType.Document);
  if Game <> nil then
    Exit(TtgMessageType.Game);
  if (Venue <> nil) then
    Exit(TtgMessageType.Venue);
  if (Location <> nil) then
    Exit(TtgMessageType.Location);
  if (NewChatMember <> nil) or (LeftChatMember <> nil) or ((NewChatPhoto <> nil) and (Length(NewChatPhoto) > 0)) or
    ((NewChatMembers <> nil) and (Length(NewChatMembers) > 0)) or (not NewChatTitle.IsEmpty) or DeleteChatPhoto or
    GroupChatCreated or SupergroupChatCreated or ChannelChatCreated or (MigrateToChatId <> 0) or
    (MigrateFromChatId <> 0) or (PinnedMessage <> nil) then
    Exit(TtgMessageType.Service);
  if (Photo <> nil) and (Length(Photo) > 0) then
    Exit(TtgMessageType.Photo);
  if (Sticker <> nil) then
    Exit(TtgMessageType.Sticker);
  if (Video <> nil) then
    Exit(TtgMessageType.Video);
  if (VideoNote <> nil) then
    Exit(TtgMessageType.VideoNote);
  if (Voice <> nil) then
    Exit(TtgMessageType.Voice);
  if not Text.IsEmpty then
    Exit(TtgMessageType.Text);
  Result := TtgMessageType.Unknown;
end;

function TTgMessage.Animation: ItgAnimation;
begin
  Result := ToClass<TtgAnimation>('animation');
end;

function TTgMessage.Audio: ItgAudio;
begin
  Result := ToClass<TtgAudio>('audio');
end;

function TTgMessage.AuthorSignature: string;
begin
  Result := ToSimpleType<string>('author_signature');
end;

function TTgMessage.Caption: string;
begin
  Result := ToSimpleType<string>('caption');
end;

function TTgMessage.CaptionEntities: TArray<ItgMessageEntity>;
begin
  Result := ToArray<ItgMessageEntity>(TtgMessageEntity, 'caption_entities');
end;

function TTgMessage.ChannelChatCreated: Boolean;
begin
  Result := ToSimpleType<Boolean>('channel_chat_created');
end;

function TTgMessage.Chat: ItgChat;
begin
  Result := ToClass<TtgChat>('chat');
end;

function TTgMessage.ConnectedWebsite: string;
begin
  Result := ToSimpleType<string>('connected_website');
end;

function TTgMessage.Contact: ItgContact;
begin
  Result := ToClass<TtgContact>('contact');
end;

function TTgMessage.Date: TDateTime;
begin
  Result := ToDateTime('date');
end;

function TTgMessage.DeleteChatPhoto: Boolean;
begin
  Result := ToSimpleType<Boolean>('delete_chat_photo');
end;

function TTgMessage.Venue: ItgVenue;
begin
  Result := ToClass<TtgVenue>('venue');
end;

function TTgMessage.Video: ItgVideo;
begin
  Result := ToClass<TtgVideo>('video');
end;

function TTgMessage.VideoNote: ItgVideoNote;
begin
  Result := ToClass<TtgVideoNote>('video_note');
end;

function TTgMessage.Voice: ItgVoice;
begin
  Result := ToClass<TtgVoice>('voice');
end;

{ TtgShippingOption }

function TtgShippingOption.ID: string;
begin
  Result := ToSimpleType<string>('id');
end;

function TtgShippingOption.Prices: TArray<ItgLabeledPrice>;
begin
  Result := ToArray<ItgLabeledPrice>(TtgLabeledPrice, 'prices');
end;

function TtgShippingOption.Title: string;
begin
  Result := ToSimpleType<string>('title');
end;

{ TtgUpdate }
function TtgUpdate.CallbackQuery: ItgCallbackQuery;
begin
  Result := ToClass<TtgCallbackQuery>('callback_query');
end;

function TtgUpdate.ChannelPost: ITgMessage;
begin
  Result := ToClass<TTgMessage>('channel_post');
end;

function TtgUpdate.ChosenInlineResult: ItgChosenInlineResult;
begin
  Result := ToClass<TtgChosenInlineResult>('chosen_inline_result');
end;

function TtgUpdate.EditedChannelPost: ITgMessage;
begin
  Result := ToClass<TTgMessage>('edited_channel_post');
end;

function TtgUpdate.EditedMessage: ITgMessage;
begin
  Result := ToClass<TTgMessage>('edited_message');
end;

function TtgUpdate.ID: Int64;
begin
  Result := ToSimpleType<Int64>('update_id');
end;

function TtgUpdate.InlineQuery: ItgInlineQuery;
begin
  Result := ToClass<TtgInlineQuery>('inline_query');
end;

function TtgUpdate.Message: ITgMessage;
begin
  Result := ToClass<TTgMessage>('message');
end;

function TtgUpdate.PreCheckoutQuery: ItgPreCheckoutQuery;
begin
  Result := ToClass<TtgPreCheckoutQuery>('pre_checkout_query');
end;

function TtgUpdate.ShippingQuery: ItgShippingQuery;
begin
  Result := ToClass<TtgShippingQuery>('shipping_query');
end;

function TtgUpdate.&Type: TtgUpdateType;
begin
  if CallbackQuery <> nil then
    Result := TtgUpdateType.CallbackQueryUpdate
  else if ChannelPost <> nil then
    Result := (TtgUpdateType.ChannelPost)
  else if ChosenInlineResult <> nil then
    Result := (TtgUpdateType.ChosenInlineResultUpdate)
  else if EditedChannelPost <> nil then
    Result := (TtgUpdateType.EditedChannelPost)
  else if EditedMessage <> nil then
    Result := (TtgUpdateType.EditedMessage)
  else if InlineQuery <> nil then
    Result := (TtgUpdateType.InlineQueryUpdate)
  else if Message <> nil then
    Result := (TtgUpdateType.MessageUpdate)
  else if PreCheckoutQuery <> nil then
    Result := (TtgUpdateType.PreCheckoutQueryUpdate)
  else if ShippingQuery <> nil then
    Result := (TtgUpdateType.ShippingQueryUpdate)
  else
    Result := TtgUpdateType.UnknownUpdate;
end;

{ TtgLocation }

constructor TtgLocation.Create(const ALongitude, ALatitude: Double);
begin
  inherited Create;
  SetLongitude(ALongitude);
  SetLatitude(ALatitude);
end;

constructor TtgLocation.Create(const AJson: string);
begin
  inherited Create(AJson);
end;

function TtgLocation.GetLatitude: Double;
begin
  Result := ToSimpleType<Double>('latitude');
end;

function TtgLocation.GetLongitude: Double;
begin
  Result := ToSimpleType<Double>('longitude');
end;

procedure TtgLocation.SetLatitude(const Value: Double);
begin
  Write('latitude', TJSONNumber.Create(Value));
end;

procedure TtgLocation.SetLongitude(const Value: Double);
begin
  Write('longitude', TJSONNumber.Create(Value));
end;

{ TtgStickerSet }

function TtgStickerSet.ContainsMasks: Boolean;
begin
  Result := ToSimpleType<Boolean>('contains_masks');
end;

function TtgStickerSet.Name: string;
begin
  Result := ToSimpleType<string>('name');
end;

function TtgStickerSet.Stickers: TArray<ItgSticker>;
begin
  Result := ToArray<ItgSticker>(TtgSticker, 'stickers');
end;

function TtgStickerSet.Title: string;
begin
  Result := ToSimpleType<string>('title');
end;

{ TtgLabeledPrice }

function TtgLabeledPrice.Amount: Int64;
begin
  Result := ToSimpleType<Int64>('amount');
end;

function TtgLabeledPrice.Text: string;
begin
  Result := ToSimpleType<string>('label');
end;

{ TtgResponseParameters }

function TtgResponseParameters.MigrateToChatId: Int64;
begin
  Result := ToSimpleType<Int64>('migrate_to_chat_id');
end;

function TtgResponseParameters.RetryAfter: Int64;
begin
  Result := ToSimpleType<Int64>('retry_after');
end;

{ TtgUser }

function TtgUser.FirstName: string;
begin
  Result := ToSimpleType<string>('first_name');
end;

function TtgUser.ID: Int64;
begin
  Result := ToSimpleType<Int64>('id');
end;

function TtgUser.IsBot: Boolean;
begin
  Result := ToSimpleType<Boolean>('is_bot');
end;

function TtgUser.LanguageCode: string;
begin
  Result := ToSimpleType<string>('language_code');
end;

function TtgUser.LastName: string;
begin
  Result := ToSimpleType<string>('last_name');
end;

function TtgUser.Username: string;
begin
  Result := ToSimpleType<string>('username');
end;

{ TtgInlineQuery }

function TtgInlineQuery.From: ItgUser;
begin
  Result := ToClass<TtgUser>('from');
end;

function TtgInlineQuery.ID: string;
begin
  Result := ToSimpleType<string>('id');
end;

function TtgInlineQuery.Offset: string;
begin
  Result := ToSimpleType<string>('offset');
end;

function TtgInlineQuery.Query: string;
begin
  Result := ToSimpleType<string>('query');
end;

{ TtgChosenInlineResult }

function TtgChosenInlineResult.From: ItgUser;
begin
  Result := ToClass<TtgUser>('from');
end;

function TtgChosenInlineResult.InlineMessageId: string;
begin
  Result := ToSimpleType<string>('inline_message_id');
end;

function TtgChosenInlineResult.Location: ItgLocation;
begin
  Result := ToClass<TtgLocation>('location');
end;

function TtgChosenInlineResult.Query: string;
begin
  Result := ToSimpleType<string>('query');
end;

function TtgChosenInlineResult.ResultId: string;
begin
  Result := ToSimpleType<string>('result_id');
end;

{ TtgPreCheckoutQuery }

function TtgPreCheckoutQuery.Currency: string;
begin
  Result := ToSimpleType<string>('currency');
end;

function TtgPreCheckoutQuery.From: ItgUser;
begin
  Result := ToClass<TtgUser>('from');
end;

function TtgPreCheckoutQuery.ID: string;
begin
  Result := ToSimpleType<string>('id');
end;

function TtgPreCheckoutQuery.InvoicePayload: string;
begin
  Result := ToSimpleType<string>('invoice_payload');
end;

function TtgPreCheckoutQuery.OrderInfo: ItgOrderInfo;
begin
  Result := ToClass<TtgOrderInfo>('order_info');
end;

function TtgPreCheckoutQuery.ShippingOptionId: string;
begin
  Result := ToSimpleType<string>('shipping_option_id');
end;

function TtgPreCheckoutQuery.TotalAmount: Int64;
begin
  Result := ToSimpleType<Int64>('total_amount');
end;

{ TtgShippingQuery }

function TtgShippingQuery.From: ItgUser;
begin
  Result := ToClass<TtgUser>('from');
end;

function TtgShippingQuery.ID: string;
begin
  Result := ToSimpleType<string>('id');
end;

function TtgShippingQuery.InvoicePayload: string;
begin
  Result := ToSimpleType<string>('invoice_payload');
end;

function TtgShippingQuery.ShippingAddress: ItgShippingAddress;
begin
  Result := ToClass<TtgShippingAddress>('shipping_address');
end;

{ TtgChatPhoto }

function TtgChatPhoto.BigFileId: string;
begin
  Result := ToSimpleType<string>('big_file_id');
end;

function TtgChatPhoto.SmallFileId: string;
begin
  Result := ToSimpleType<string>('small_file_id');
end;

{ TtgChatMember }

function TtgChatMember.CanAddWebPagePreviews: Boolean;
begin
  Result := ToSimpleType<Boolean>('can_add_web_page_previews');
end;

function TtgChatMember.CanBeEdited: Boolean;
begin
  Result := ToSimpleType<Boolean>('can_change_info');
end;

function TtgChatMember.CanChangeInfo: Boolean;
begin
  Result := ToSimpleType<Boolean>('can_change_info');
end;

function TtgChatMember.CanDeleteMessages: Boolean;
begin
  Result := ToSimpleType<Boolean>('can_delete_messages');
end;

function TtgChatMember.CanEditMessages: Boolean;
begin
  Result := ToSimpleType<Boolean>('can_edit_messages');
end;

function TtgChatMember.CanInviteUsers: Boolean;
begin
  Result := ToSimpleType<Boolean>('can_invite_users');
end;

function TtgChatMember.CanPinMessages: Boolean;
begin
  Result := ToSimpleType<Boolean>('can_pin_messages');
end;

function TtgChatMember.CanPostMessages: Boolean;
begin
  Result := ToSimpleType<Boolean>('can_post_messages');
end;

function TtgChatMember.CanPromoteMembers: Boolean;
begin
  Result := ToSimpleType<Boolean>('can_promote_members');
end;

function TtgChatMember.CanRestrictMembers: Boolean;
begin
  Result := ToSimpleType<Boolean>('can_restrict_members');
end;

function TtgChatMember.CanSendMediaMessages: Boolean;
begin
  Result := ToSimpleType<Boolean>('can_send_media_messages');
end;

function TtgChatMember.CanSendMessages: Boolean;
begin
  Result := ToSimpleType<Boolean>('can_send_messages');
end;

function TtgChatMember.CanSendOtherMessages: Boolean;
begin
  Result := ToSimpleType<Boolean>('can_send_other_messages');
end;

function TtgChatMember.Status: TtgChatMemberStatus;
var
  LStatus: string;
begin
  Result := TtgChatMemberStatus.Member;
  LStatus := ToSimpleType<string>('status');
  if LStatus = 'creator' then
    Result := TtgChatMemberStatus.Creator
  else if LStatus = 'administrator' then
    Result := TtgChatMemberStatus.Administrator
  else if LStatus = 'member' then
    Result := TtgChatMemberStatus.Member
  else if LStatus = 'restricted' then
    Result := TtgChatMemberStatus.Restricted
  else if LStatus = 'left' then
    Result := TtgChatMemberStatus.Left
  else if LStatus = 'kicked' then
    Result := TtgChatMemberStatus.Kicked
  else
    TBaseJson.UnSupported;
end;

function TtgChatMember.UntilDate: TDateTime;
begin
  Result := ToDateTime('until_date');
end;

function TtgChatMember.User: ItgUser;
begin
  Result := ToClass<TtgUser>('user');
end;

{ TtgChat }

function TtgChat.AllMembersAreAdministrators: Boolean;
begin
  Result := ToSimpleType<Boolean>('all_members_are_administrators');
end;

function TtgChat.CanSetStickerSet: Boolean;
begin
  Result := ToSimpleType<Boolean>('can_set_sticker_set');
end;

function TtgChat.Description: string;
begin
  Result := ToSimpleType<string>('description');
end;

function TtgChat.FirstName: string;
begin
  Result := ToSimpleType<string>('first_name');
end;

function TtgChat.ID: Int64;
begin
  Result := ToSimpleType<Int64>('id');
end;

function TtgChat.InviteLink: string;
begin
  Result := ToSimpleType<string>('invite_link');
end;

function TtgChat.LastName: string;
begin
  Result := ToSimpleType<string>('last_name');
end;

function TtgChat.Photo: ItgChatPhoto;
begin
  Result := ToClass<TtgChatPhoto>('photo');
end;

function TtgChat.PinnedMessage: ITgMessage;
begin
  Result := ToClass<TTgMessage>('pinned_message');
end;

function TtgChat.StickerSetName: string;
begin
  Result := ToSimpleType<string>('sticker_set_name');
end;

function TtgChat.Title: string;
begin
  Result := ToSimpleType<string>('title');
end;

function TtgChat.TypeChat: TtgChatType;
var
  LValue: string;
begin
  LValue := ToSimpleType<string>('type');
  Result := TtgChatType.&private;
  if LValue = 'private' then
    Result := TtgChatType.&private
  else if LValue = 'group' then
    Result := TtgChatType.Group
  else if LValue = 'channel' then
    Result := TtgChatType.Channel
  else if LValue = 'supergroup' then
    Result := TtgChatType.Supergroup
  else
    UnSupported;
end;

function TtgChat.Username: string;
begin
  Result := ToSimpleType<string>('username');
end;

{ TtgSuccessfulPayment }

function TtgSuccessfulPayment.Currency: string;
begin
  Result := ToSimpleType<string>('currency');
end;

function TtgSuccessfulPayment.InvoicePayload: string;
begin
  Result := ToSimpleType<string>('invoice_payload');
end;

function TtgSuccessfulPayment.OrderInfo: ItgOrderInfo;
begin
  Result := ToClass<TtgOrderInfo>('order_info');
end;

function TtgSuccessfulPayment.ProviderPaymentChargeId: string;
begin
  Result := ToSimpleType<string>('provider_payment_charge_id');
end;

function TtgSuccessfulPayment.ShippingOptionId: string;
begin
  Result := ToSimpleType<string>('shipping_option_id');
end;

function TtgSuccessfulPayment.TelegramPaymentChargeId: string;
begin
  Result := ToSimpleType<string>('telegram_payment_charge_id');
end;

function TtgSuccessfulPayment.TotalAmount: Int64;
begin
  Result := ToSimpleType<Int64>('total_amount');
end;

{ TtgWebhookInfo }

function TtgWebhookInfo.AllowedUpdates: TArray<string>;
var
  LJsonArray: TJSONArray;
  I: Integer;
begin
  LJsonArray := GetJson.GetValue('allowed_updates') as TJSONArray;
  if (not Assigned(LJsonArray)) or LJsonArray.Null then
    Exit(nil);
  SetLength(Result, LJsonArray.Count);
  for I := 0 to LJsonArray.Count - 1 do
    Result[I] := ToSimpleType<string>(LJsonArray.Items[I].ToString);
end;

function TtgWebhookInfo.HasCustomCertificate: Boolean;
begin
  Result := ToSimpleType<Boolean>('has_custom_certificate');
end;

function TtgWebhookInfo.LastErrorDate: TDateTime;
begin
  Result := ToDateTime('last_error_date');
end;

function TtgWebhookInfo.LastErrorMessage: string;
begin
  Result := ToSimpleType<string>('last_error_message');
end;

function TtgWebhookInfo.MaxConnections: Int64;
begin
  Result := ToSimpleType<Int64>('max_connections');
end;

function TtgWebhookInfo.PendingUpdateCount: Int64;
begin
  Result := ToSimpleType<Int64>('pending_update_count');
end;

function TtgWebhookInfo.Url: string;
begin
  Result := ToSimpleType<string>('url');
end;

{ TtgMessageEntity }

function TtgMessageEntity.Length: Int64;
begin
  Result := ToSimpleType<Int64>('length');
end;

function TtgMessageEntity.Offset: Int64;
begin
  Result := ToSimpleType<Int64>('offset');
end;

function TtgMessageEntity.TypeMessage: TtgMessageEntityType;
var
  LValue: string;
begin
  LValue := ToSimpleType<string>('type');
  if LValue = 'mention' then
    Result := TtgMessageEntityType.Mention
  else if LValue = 'hashtag' then
    Result := TtgMessageEntityType.Hashtag
  else if LValue = 'bot_command' then
    Result := TtgMessageEntityType.BotCommand
  else if LValue = 'url' then
    Result := TtgMessageEntityType.Url
  else if LValue = 'bold' then
    Result := TtgMessageEntityType.Bold
  else if LValue = 'italic' then
    Result := TtgMessageEntityType.Italic
  else if LValue = 'code' then
    Result := TtgMessageEntityType.Code
  else if LValue = 'pre' then
    Result := TtgMessageEntityType.Pre
  else if LValue = 'text_link' then
    Result := TtgMessageEntityType.TextLink
  else if LValue = 'text_mention' then
    Result := TtgMessageEntityType.TextMention
  else if LValue = 'email' then
    Result := TtgMessageEntityType.Email
  else if LValue = 'cashtag' then
    Result := TtgMessageEntityType.CashTag
  else if LValue = 'phone_number' then
    Result := TtgMessageEntityType.PhoneNumber
  else
    raise Exception.CreateFmt('Cant parse Entity: %S', [LValue]);
end;

function TtgMessageEntity.Url: string;
begin
  Result := ToSimpleType<string>('url');
end;

function TtgMessageEntity.User: ItgUser;
begin
  Result := ToClass<TtgUser>('user');
end;

{ TtgAudio }

function TtgAudio.Duration: Int64;
begin
  Result := ToSimpleType<Int64>('duration');
end;

function TtgAudio.MimeType: string;
begin
  Result := ToSimpleType<string>('mime_type');
end;

function TtgAudio.Performer: string;
begin
  Result := ToSimpleType<string>('performer');
end;

function TtgAudio.Thumb: ItgPhotoSize;
begin
  Result := ToClass<TtgPhotoSize>('thumb');
end;

function TtgAudio.Title: string;
begin
  Result := ToSimpleType<string>('title');
end;

{ TtgPhotoSize }

function TtgPhotoSize.Height: Int64;
begin
  Result := ToSimpleType<Int64>('height');
end;

function TtgPhotoSize.Width: Int64;
begin
  Result := ToSimpleType<Int64>('width');
end;

{ TtgMaskPosition }

function TtgMaskPosition.Point: TtgMaskPositionPoint;
var
  LValue: string;
begin
  LValue := ToSimpleType<string>('point');
  Result := TtgMaskPositionPoint.forehead;
  if LValue = 'forehead' then
    Result := TtgMaskPositionPoint.forehead
  else if LValue = 'eyes' then
    Result := TtgMaskPositionPoint.eyes
  else if LValue = 'mouth' then
    Result := TtgMaskPositionPoint.mouth
  else if LValue = 'chin' then
    Result := TtgMaskPositionPoint.chin
  else
    UnSupported;
end;

function TtgMaskPosition.Scale: Single;
begin
  Result := ToSimpleType<Single>('scale');
end;

function TtgMaskPosition.XShift: Single;
begin
  Result := ToSimpleType<Single>('x_shift');
end;

function TtgMaskPosition.YShift: Single;
begin
  Result := ToSimpleType<Single>('y_shift');
end;

{ TtgSticker }

function TtgSticker.Emoji: string;
begin
  Result := ToSimpleType<string>('emoji');
end;

function TtgSticker.Height: Int64;
begin
  Result := ToSimpleType<Int64>('height');
end;

function TtgSticker.MaskPosition: ItgMaskPosition;
begin
  Result := ToClass<TtgMaskPosition>('mask_position');
end;

function TtgSticker.SetName: string;
begin
  Result := ToSimpleType<string>('set_name');
end;

function TtgSticker.Thumb: ItgPhotoSize;
begin
  Result := ToClass<TtgPhotoSize>('thumb');
end;

function TtgSticker.Width: Int64;
begin
  Result := ToSimpleType<Int64>('width');
end;

{ TtgGame }

function TtgGame.Animation: ItgAnimation;
begin
  Result := ToClass<TtgAnimation>('animation');
end;

function TtgGame.Description: string;
begin
  Result := ToSimpleType<string>('description');
end;

function TtgGame.Photo: TArray<ItgPhotoSize>;
begin
  Result := ToArray<ItgPhotoSize>(TtgPhotoSize, 'photo');
end;

function TtgGame.Text: string;
begin
  Result := ToSimpleType<string>('text');
end;

function TtgGame.TextEntities: TArray<ItgMessageEntity>;
begin
  Result := ToArray<ItgMessageEntity>(TtgMessageEntity, 'text_entities');
end;

function TtgGame.Title: string;
begin
  Result := ToSimpleType<string>('title');
end;

{ TtgInvoice }

function TtgInvoice.Currency: string;
begin
  Result := ToSimpleType<string>('currency');
end;

function TtgInvoice.Description: string;
begin
  Result := ToSimpleType<string>('description');
end;

function TtgInvoice.StartParameter: string;
begin
  Result := ToSimpleType<string>('start_parameter');
end;

function TtgInvoice.Title: string;
begin
  Result := ToSimpleType<string>('title');
end;

function TtgInvoice.TotalAmount: Int64;
begin
  Result := ToSimpleType<Int64>('total_amount');
end;

{ TtgVideo }

function TtgVideo.Duration: Int64;
begin
  Result := ToSimpleType<Int64>('duration');
end;

function TtgVideo.Height: Int64;
begin
  Result := ToSimpleType<Int64>('height');
end;

function TtgVideo.MimeType: string;
begin
  Result := ToSimpleType<string>('mime_type');
end;

function TtgVideo.Thumb: ItgPhotoSize;
begin
  Result := ToClass<TtgPhotoSize>('thumb');
end;

function TtgVideo.Width: Int64;
begin
  Result := ToSimpleType<Int64>('width');
end;

{ TtgContact }

constructor TtgContact.Create(const AFirstName, ALastName, APhoneNumber: string);
begin
  inherited Create();
  FirstName := AFirstName;
  LastName := ALastName;
  PhoneNumber := APhoneNumber;
end;

function TtgContact.GetFirstName: string;
begin
  Result := ToSimpleType<string>('first_name');
end;

function TtgContact.GetLastName: string;
begin
  Result := ToSimpleType<string>('last_name');
end;

function TtgContact.GetPhoneNumber: string;
begin
  Result := ToSimpleType<string>('phone_number');
end;

function TtgContact.GetUserId: Int64;
begin
  Result := ToSimpleType<Int64>('user_id');
end;

function TtgContact.GetvCard: string;
begin
  Result := ToSimpleType<string>('vcard');
end;

procedure TtgContact.SetFirstName(const AValue: string);
begin
  Write('first_name', AValue);
end;

procedure TtgContact.SetLastName(const AValue: string);
begin
  Write('last_name', AValue);
end;

procedure TtgContact.SetPhoneNumber(const AValue: string);
begin
  Write('phone_number', AValue);
end;

procedure TtgContact.SetUserId(const AValue: Int64);
begin
  Write('user_id', TJSONNumber.Create(AValue));
end;

procedure TtgContact.SetvCard(const Value: string);
begin
  Write('vcard', Value);
end;

{ TtgVenue }

function TtgVenue.GetAddress: string;
begin
  Result := ToSimpleType<string>('address');
end;

function TtgVenue.GetFoursquareId: string;
begin
  Result := ToSimpleType<string>('foursquare_id');
end;

function TtgVenue.GetFoursquareType: string;
begin
  Result := ToSimpleType<string>('foursquare_type');
end;

function TtgVenue.GetLocation: ItgLocation;
begin
  Result := ToClass<TtgLocation>('location');
end;

function TtgVenue.GetTitle: string;
begin
  Result := ToSimpleType<string>('title');
end;

procedure TtgVenue.SetAddress(const AValue: string);
begin
  Write('address', AValue);
end;

procedure TtgVenue.SetFoursquareId(const AValue: string);
begin
  Write('foursquare_id', AValue);
end;

procedure TtgVenue.SetFoursquareType(const Value: string);
begin
  Write('foursquare_type', Value);
end;

procedure TtgVenue.SetLocation(const AValue: ItgLocation);
begin
  Write('location', (AValue as TtgLocation).GetJson);
end;

procedure TtgVenue.SetTitle(const AValue: string);
begin
  Write('title', AValue);
end;

{ TtgVideoNote }

function TtgVideoNote.Duration: Int64;
begin
  Result := ToSimpleType<Int64>('duration');
end;

function TtgVideoNote.FileId: string;
begin
  Result := ToSimpleType<string>('file_id');
end;

function TtgVideoNote.FileSize: Int64;
begin
  Result := ToSimpleType<Int64>('file_size');
end;

function TtgVideoNote.Length: Int64;
begin
  Result := ToSimpleType<Int64>('length');
end;

function TtgVideoNote.Thumb: ItgPhotoSize;
begin
  Result := ToClass<TtgPhotoSize>('thumb');
end;

{ TtgVoice }

function TtgVoice.Duration: Int64;
begin
  Result := ToSimpleType<Int64>('duration');
end;

function TtgVoice.MimeType: string;
begin
  Result := ToSimpleType<string>('mime_type');
end;

{ TtgOrderInfo }

function TtgOrderInfo.Email: string;
begin
  Result := ToSimpleType<string>('email');
end;

function TtgOrderInfo.Name: string;
begin
  Result := ToSimpleType<string>('name');
end;

function TtgOrderInfo.PhoneNumber: string;
begin
  Result := ToSimpleType<string>('phone_number');
end;

function TtgOrderInfo.ShippingAddress: ItgShippingAddress;
begin
  Result := ToClass<TtgShippingAddress>('shipping_address');
end;

{ TtgShippingAddress }

function TtgShippingAddress.City: string;
begin
  Result := ToSimpleType<string>('city');
end;

function TtgShippingAddress.CountryCode: string;
begin
  Result := ToSimpleType<string>('country_code');
end;

function TtgShippingAddress.PostCode: string;
begin
  Result := ToSimpleType<string>('post_code');
end;

function TtgShippingAddress.State: string;
begin
  Result := ToSimpleType<string>('state');
end;

function TtgShippingAddress.StreetLine1: string;
begin
  Result := ToSimpleType<string>('street_line1');
end;

function TtgShippingAddress.StreetLine2: string;
begin
  Result := ToSimpleType<string>('street_line2');
end;

{ TtgUserProfilePhotos }

function TtgUserProfilePhotos.Photos: TArray<TArray<ItgPhotoSize>>;
var
  PhotoArr, SizeArr: TJSONArray;
  PhotoIndex, ResultPhotoIndex: Integer;
  SizeIndex: Integer;
  GUID: TGUID;
  LCode: string;
  LFS: TtgPhotoSize;
begin
  Result := nil;
  PhotoArr := GetJson.GetValue('photos') as TJSONArray;
  if (not Assigned(PhotoArr)) or PhotoArr.Null then
    Exit;
  GUID := GetTypeData(TypeInfo(ItgPhotoSize))^.GUID;
  SetLength(Result, PhotoArr.Count);
  // Some photos could be empty(?), so we should
  // use separated counter instead of copy of the FOR-loop variable value.
  ResultPhotoIndex := 0;
  for PhotoIndex := 0 to High(Result) do
  begin
    // get array of photoSizes from photoArr[i]
    SizeArr := PhotoArr.Items[PhotoIndex] as TJSONArray;
    // check for empty photo
    if (not Assigned(SizeArr)) or SizeArr.Null then
      Continue;
    // set length of photoSize array
    SetLength(Result[ResultPhotoIndex], SizeArr.Count);
    // fills the result[RealIndex] with array of sizes
    for SizeIndex := 0 to High(Result[ResultPhotoIndex]) do
    begin
      LCode := SizeArr.Items[SizeIndex].ToString;
      LFS := TBaseJsonClass(TtgPhotoSize).Create(LCode) as TtgPhotoSize;
      LFS.GetInterface(GUID, Result[ResultPhotoIndex, SizeIndex]);
    end;
    // inc counter of processed photos
    Inc(ResultPhotoIndex);
  end;
  // Set real length of the result array. length = zero based index + 1;
  SetLength(Result, ResultPhotoIndex + 1);
end;

function TtgUserProfilePhotos.TotalCount: Int64;
begin
  Result := ToSimpleType<Int64>('total_count');
end;

end.
