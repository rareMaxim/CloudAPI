unit TelegAPi.Types;

interface

uses
  XSuperObject,
  System.Classes;

Type

{$SCOPEDENUMS ON}
  /// <summary>
  /// The type of a Message
  /// </summary>
  TTelegaMessageType = (UnknownMessage = 0, TextMessage, PhotoMessage, AudioMessage, VideoMessage,
    VoiceMessage, DocumentMessage, StickerMessage, LocationMessage, ContactMessage, ServiceMessage,
    VenueMessage);
  /// <summary>
  /// Text parsing mode
  /// </summary>
  TTelegaParseMode = (Default = 0, Markdown, Html);
  /// <summary>
  /// The type of a Message
  /// </summary>

{$SCOPEDENUMS OFF}

  TTelegaReplyMarkup = Class
  private
    FSelective: Boolean;
  published
    /// <summary>
    /// Optional. Use this parameter if you want to show the keyboard to specific users only. Targets: 1) users that are @mentioned in the text of the Message object; 2) if the bot's message is a reply (has reply_to_message_id), sender of the original message.
    /// Example: A user requests to change the bot‘s language, bot replies to the request with a keyboard to select the new language.Other users in the group don’t see the keyboard.
    /// </summary>
    [ALIAS('selective')]
    property Selective: Boolean read FSelective write FSelective;
  End;

  /// <summary>
  /// This object represents a point on the map.
  /// </summary>
  TTelegaLocation = Class
  private
    FLongitude: Single;
    FLatitude: Single;
  published
    /// <summary>
    /// Longitude as defined by sender
    /// </summary>
    [ALIAS('longitude')]
    property Longitude: Single read FLongitude write FLongitude;
    /// <summary>
    /// Latitude as defined by sender
    /// </summary>
    [ALIAS('latitude')]
    property Latitude: Single read FLatitude write FLatitude;
  End;

  /// <summary>
  /// This object represents a venue.
  /// </summary>
  TTelegaVenue = Class
  private
    FLocation: TTelegaLocation;
    FTitle: String;
    FAddress: String;
    FFoursquareId: String;
  published
    /// <summary>
    /// Venue location
    /// </summary>
    [ALIAS('location')]
    property Location: TTelegaLocation read FLocation write FLocation;
    /// <summary>
    /// Title of the result
    /// </summary>
    [ALIAS('title')]
    property Title: String read FTitle write FTitle;
    /// <summary>
    /// Address of the venue
    /// </summary>
    [ALIAS('address')]
    property Address: String read FAddress write FAddress;
    /// <summary>
    /// Optional. Foursquare identifier of the venue
    /// </summary>
    [ALIAS('foursquare_id')]
    property FoursquareId: String read FFoursquareId write FFoursquareId;
  End;

  TTelegaContact = Class
  private
    FPhoneNumber: String;
    FFirstName: String;
    FLastName: String;
    FUserId: Integer;
  published
    /// <summary>
    /// Contact's phone number
    /// </summary>
    [ALIAS('phone_number')]
    property PhoneNumber: String read FPhoneNumber write FPhoneNumber;
    /// <summary>
    /// Contact's first name
    /// </summary>
    [ALIAS('first_name')]
    property FirstName: String read FFirstName write FFirstName;
    /// <summary>
    /// Optional. Contact's last name
    /// </summary>
    [ALIAS('last_name')]
    property LastName: String read FLastName write FLastName;
    /// <summary>
    /// Optional. Contact's user identifier in Telegram
    /// </summary>
    [ALIAS('user_id')]
    property UserId: Integer read FUserId write FUserId;
  End;

  TTelegaFile = Class
  private
    FFileId: String;
    FFileSize: Integer;
    FFilePath: String;
    FFileStream: TStream;
  published
    /// <summary>
    /// Unique identifier for this file
    /// </summary>
    [ALIAS('file_id')]
    property FileId: String read FFileId write FFileId;
    /// <summary>
    /// Optional. File size, if known
    /// </summary>
    [ALIAS('file_size')]
    property FileSize: Integer read FFileSize write FFileSize;
    /// <summary>
    /// File path. Use https://api.telegram.org/file/bot{token}/{file_path} to get the file.
    /// </summary>
    [ALIAS('file_path')]
    property FilePath: String read FFilePath write FFilePath;
    property FileStream: TStream read FFileStream write FFileStream;
  End;

  TTelegaAudio = Class(TTelegaFile)
  private
    FDuration: Integer;
    FPerformer: String;
    FTitle: String;
    FMimeType: String;
  published
    /// <summary>
    /// Duration of the audio in seconds as defined by sender
    /// </summary>
    [ALIAS('duration')]
    property Duration: Integer read FDuration write FDuration;
    /// <summary>
    /// Performer of the audio as defined by sender or by audio tags
    /// </summary>
    [ALIAS('performer')]
    property Performer: String read FPerformer write FPerformer;
    /// <summary>
    /// Title of the audio as defined by sender or by audio tags
    /// </summary>
    [ALIAS('title')]
    property Title: String read FTitle write FTitle;
    /// <summary>
    /// Optional. MIME type of the file as defined by sender
    /// </summary>
    [ALIAS('mime_type')]
    property MimeType: String read FMimeType write FMimeType;
  End;

  /// <summary>
  /// This object represents one size of a photo or a file / sticker thumbnail.
  /// </summary>
  /// <remarks>A missing thumbnail for a file (or sticker) is presented as an empty object.</remarks>
  ttelegaPhotoSize = Class(TTelegaFile)
  private
    FWidth: Integer;
    FHeight: Integer;
  published
    /// <summary>
    /// Photo width
    /// </summary>
    [ALIAS('width')]
    property Width: Integer read FWidth write FWidth;
    /// <summary>
    /// Photo height
    /// </summary>
    [ALIAS('Height')]
    property Height: Integer read FHeight write FHeight;
  End;

  TTelegaUserProfilePhotos = Class
  private
    Ftotal_count: Integer;
    Fphotos: TArray<TArray<ttelegaPhotoSize>>;
  published
    [ALIAS('total_count')]
    property total_count: Integer read Ftotal_count write Ftotal_count;
    property photos: TArray < TArray < ttelegaPhotoSize >> read Fphotos write Fphotos;
  End;

  TTelegaDocument = Class(TTelegaFile)
  private
    FThumb: ttelegaPhotoSize;
    FFileName: String;
    FMimeType: String;
  published
    /// <summary>
    /// Document thumbnail as defined by sender
    /// </summary>
    [ALIAS('thumb')]
    property Thumb: ttelegaPhotoSize read FThumb write FThumb;
    /// <summary>
    /// Optional. Original filename as defined by sender
    /// </summary>
    [ALIAS('file_name')]
    property FileName: String read FFileName write FFileName;
    /// <summary>
    /// Optional. MIME type of the file as defined by sender
    /// </summary>
    [ALIAS('mime_type')]
    property MimeType: String read FMimeType write FMimeType;
  End;

  TTelegaSticker = Class(TTelegaFile)
  private
    FWidth: Integer;
    FHeight: Integer;
    FThumb: ttelegaPhotoSize;
  published
    /// <summary>
    /// Sticker width
    /// </summary>
    [ALIAS('width')]
    property Width: Integer read FWidth write FWidth;
    /// <summary>
    /// Sticker height
    /// </summary>
    [ALIAS('width')]
    property Height: Integer read FHeight write FHeight;
    /// <summary>
    /// Sticker thumbnail in .webp or .jpg format
    /// </summary>
    [ALIAS('thumb')]
    property Thumb: ttelegaPhotoSize read FThumb write FThumb;

  End;

  TTelegaVideo = Class(TTelegaFile)
  private
    FWidth: String;
    FHeight: String;
    FDuration: Integer;
    FThumb: ttelegaPhotoSize;
    FMimeType: String;
  published
    /// <summary>
    /// Video width as defined by sender
    /// </summary>
    [ALIAS('width')]
    property Width: String read FWidth write FWidth;
    /// <summary>
    /// Video height as defined by sender
    /// </summary>
    [ALIAS('height')]
    property Height: String read FHeight write FHeight;
    /// <summary>
    /// Duration of the video in seconds as defined by sender
    /// </summary>
    [ALIAS('duration')]
    property Duration: Integer read FDuration write FDuration;
    /// <summary>
    /// Video thumbnail
    /// </summary>
    [ALIAS('thumb')]
    property Thumb: ttelegaPhotoSize read FThumb write FThumb;
    /// <summary>
    /// Optional. Mime type of a file as defined by sender
    /// </summary>
    [ALIAS('mime_type')]
    property MimeType: String read FMimeType write FMimeType;
  End;

  TTelegaVoice = Class(TTelegaFile)
  private
    FDuration: Integer;
    FMimeType: String;
  published
    /// <summary>
    /// Duration of the audio in seconds as defined by sender
    /// </summary>
    [ALIAS('duration')]
    property Duration: Integer read FDuration write FDuration;
    /// <summary>
    /// Optional. MIME type of the file as defined by sender
    /// </summary>
    [ALIAS('mime_type')]
    property MimeType: String read FMimeType write FMimeType;
  End;

  TTelegaUser = Class
  private
    FID: Integer;
    FFirstName: String;
    FLastName: String;
    FUsername: String;
  published
    /// <summary>
    /// Unique identifier for this user or bot
    /// </summary>
    /// <returns></returns>
    [ALIAS('id')]
    property ID: Integer read FID write FID;
    /// <summary>
    /// User‘s or bot’s first name
    /// </summary>
    [ALIAS('first_name')]
    property FirstName: String read FFirstName write FFirstName;
    /// <summary>
    /// Optional. User‘s or bot’s last name
    /// </summary>
    [ALIAS('last_name')]
    property LastName: String read FLastName write FLastName;
    /// <summary>
    /// Optional. User‘s or bot’s username
    /// </summary>
    [ALIAS('username')]
    property Username: String read FUsername write FUsername;
  End;

  /// <summary>
  /// This object represents a chat.
  /// </summary>
  TTelegaChat = Class
  private
    FID: Int64;
  published
    /// <summary>
    /// Unique identifier for this chat, not exceeding 1e13 by absolute value
    /// </summary>
    [ALIAS('id')]
    property ID: Int64 read FID write FID;

  End;

  TTelegaMessage = Class
  private
    FMessageId: Integer;
    FFrom: TTelegaUser;
    FDate: TDateTime;
    FChat: TTelegaChat;
    FForwardFrom: TTelegaUser;
    FForwardDate: TDateTime;
    FReplyToMessage: TTelegaMessage;
    FText: String;
    FAudio: TTelegaAudio;
    FDocument: TTelegaDocument;
    FPhoto: TArray<ttelegaPhotoSize>;
    FSticker: TTelegaSticker;
    FVideo: TTelegaVideo;
    FVoice: TTelegaVoice;
    FCaption: String;
    FContact: TTelegaContact;
    FLocation: TTelegaLocation;
    FVenue: TTelegaVenue;
    FNewChatMember: TTelegaUser;
    FLeftChatMember: TTelegaUser;
    FNewChatTitle: String;
    FNewChatPhoto: TArray<ttelegaPhotoSize>;
    FDeleteChatPhoto: Boolean;
    FGroupChatCreated: Boolean;
    FSupergroupChatCreated: Boolean;
    FChannelChatCreated: Boolean;
    FMigrateToChatId: Int64;
    FMigrateFromChatId: Int64;
    FPinnedMessage: TTelegaMessage;
    function GetType: TTelegaMessageType;
  public
    /// <summary>
    /// Unique message identifier
    /// </summary>
    [ALIAS('message_id')]
    property MessageId: Integer read FMessageId write FMessageId;
    /// <summary>
    /// Sender
    /// </summary>
    [ALIAS('from')]
    property From: TTelegaUser read FFrom write FFrom;
    /// <summary>
    /// Date the message was sent in Unix time
    /// </summary>
    [ALIAS('date')]
    property Date: TDateTime read FDate write FDate;
    /// <summary>
    /// Conversation the message belongs to
    /// </summary>
    [ALIAS('chat')]
    property Chat: TTelegaChat read FChat write FChat;
    /// <summary>
    /// Optional. For forwarded messages, sender of the original message
    /// </summary>
    [ALIAS('forward_from')]
    property ForwardFrom: TTelegaUser read FForwardFrom write FForwardFrom;
    /// <summary>
    /// Optional. For forwarded messages, date the original message was sent in Unix time
    /// </summary>
    [ALIAS('forward_date')]
    property ForwardDate: TDateTime read FForwardDate write FForwardDate;
    /// <summary>
    /// Optional. For replies, the original message. Note that the Message object in this field will not contain further reply_to_message fields even if it itself is a reply.
    /// </summary>
    [ALIAS('reply_to_message')]
    property ReplyToMessage: TTelegaMessage read FReplyToMessage write FReplyToMessage;
    /// <summary>
    /// Optional. For text messages, the actual UTF-8 text of the message
    /// </summary>
    [ALIAS('text')]
    property Text: String read FText write FText;
    /// <summary>
    /// Optional. Message is an audio file, information about the file
    /// </summary>
    [ALIAS('audio')]
    property Audio: TTelegaAudio read FAudio write FAudio;
    /// <summary>
    /// Optional. Message is a general file, information about the file
    /// </summary>
    [ALIAS('document')]
    property Document: TTelegaDocument read FDocument write FDocument;
    /// <summary>
    /// Optional. Message is a photo, available sizes of the photo
    /// </summary>
    [ALIAS('photo')]
    property Photo: TArray<ttelegaPhotoSize> read FPhoto write FPhoto;
    /// <summary>
    /// Optional. Message is a sticker, information about the sticker
    /// </summary>
    [ALIAS('sticker')]
    property Sticker: TTelegaSticker read FSticker write FSticker;
    /// <summary>
    /// Optional. Message is a video, information about the video
    /// </summary>
    [ALIAS('video')]
    property Video: TTelegaVideo read FVideo write FVideo;
    /// <summary>
    /// Message is a voice message, information about the file
    /// </summary>
    [ALIAS('voice')]
    property Voice: TTelegaVoice read FVoice write FVoice;
    /// <summary>
    /// Caption for the photo or video
    /// </summary>
    [ALIAS('caption')]
    property Caption: String read FCaption write FCaption;
    /// <summary>
    /// Optional. Message is a shared contact, information about the contact
    /// </summary>
    [ALIAS('contact')]
    property Contact: TTelegaContact read FContact write FContact;
    /// <summary>
    /// Optional. Message is a shared location, information about the location
    /// </summary>
    [ALIAS('location')]
    property Location: TTelegaLocation read FLocation write FLocation;
    /// <summary>
    /// Optional. Message is a venue, information about the venue
    /// </summary>
    [ALIAS('venue')]
    property Venue: TTelegaVenue read FVenue write FVenue;
    /// <summary>
    /// Optional. A new member was added to the group, information about them (this member may be bot itself)
    /// </summary>
    [ALIAS('new_chat_member')]
    property NewChatMember: TTelegaUser read FNewChatMember write FNewChatMember;
    /// <summary>
    /// Optional. A member was removed from the group, information about them (this member may be bot itself)
    /// </summary>
    [ALIAS('left_chat_member')]
    property LeftChatMember: TTelegaUser read FLeftChatMember write FLeftChatMember;
    /// <summary>
    /// Optional. A group title was changed to this value
    /// </summary>
    [ALIAS('new_chat_title')]
    property NewChatTitle: String read FNewChatTitle write FNewChatTitle;
    /// <summary>
    /// Optional. A group photo was change to this value
    /// </summary>
    [ALIAS('new_chat_photo')]
    property NewChatPhoto: TArray<ttelegaPhotoSize> read FNewChatPhoto write FNewChatPhoto;
    /// <summary>
    /// Optional. Informs that the group photo was deleted
    /// </summary>
    [ALIAS('delete_chat_photo')]
    property DeleteChatPhoto: Boolean read FDeleteChatPhoto write FDeleteChatPhoto;
    /// <summary>
    /// Optional. Informs that the group has been created
    /// </summary>
    [ALIAS('group_chat_created')]
    property GroupChatCreated: Boolean read FGroupChatCreated write FGroupChatCreated;
    /// <summary>
    /// Optional. Service message: the supergroup has been created
    /// </summary>
    [ALIAS('supergroup_chat_created')]
    property SupergroupChatCreated: Boolean read FSupergroupChatCreated
      write FSupergroupChatCreated;
    /// <summary>
    /// Optional. Service message: the channel has been created
    /// </summary>
    [ALIAS('channel_chat_created')]
    property ChannelChatCreated: Boolean read FChannelChatCreated write FChannelChatCreated;
    /// <summary>
    /// Optional. The group has been migrated to a supergroup with the specified identifier
    /// </summary>
    [ALIAS('migrate_to_chat_id')]
    property MigrateToChatId: Int64 read FMigrateToChatId write FMigrateToChatId;
    /// <summary>
    /// Optional. The supergroup has been migrated from a group with the specified identifier
    /// </summary>
    [ALIAS('migrate_from_chat_id')]
    property MigrateFromChatId: Int64 read FMigrateFromChatId write FMigrateFromChatId;
    /// <summary>
    /// Optional. Specified message was pinned. Note that the Message object in this field will not contain further reply_to_message fields even if it is itself a reply
    /// </summary>
    [ALIAS('pinned_message')]
    property PinnedMessage: TTelegaMessage read FPinnedMessage write FPinnedMessage;
    property &Type: TTelegaMessageType read GetType;
  End;

  TTelegaApiResponse<T> = Class
  private
    FOk: Boolean;
    FResultObject: T;
    FMessage: String;
    FCode: Integer;
  published
    // <summary>
    /// Gets a value indicating whether the request was successful.
    /// </summary>
    /// <value>
    /// <c>true</c> if the request was successful and the result of the query can be found in the ‘result’ field, otherwise <c>false</c>.
    /// </value>
    [ALIAS('ok')]
    property Ok: Boolean read FOk write FOk;
    /// <summary>
    /// Gets the result object.
    /// </summary>
    /// <value>
    /// The result object.
    /// </value>
    [ALIAS('result')]
    property ResultObject: T read FResultObject write FResultObject;
    /// <summary>
    /// Gets the error message.
    /// </summary>
    /// <value>
    /// The error message.
    /// </value>
    [ALIAS('description')]
    property Message: String read FMessage write FMessage;
    /// <summary>
    /// Gets the error code.
    /// </summary>
    /// <value>
    /// The error code.
    /// </value>
    [ALIAS('error_code')]
    property Code: Integer read FCode write FCode;
  End;

  TTelegaFileToSend = Class
  private
    FFileName: String;
    FContent: TStream;
  public
    constructor Create(Const FileName: String; Const Content: TStream);
    destructor Destroy; override;
  published
    property FileName: String read FFileName write FFileName;
    property Content: TStream read FContent write FContent;
  end;

  /// <summary>
  /// This object represents an incoming inline query. When the user sends an empty query, your bot could return some default or trending results.
  /// </summary>
  TTelegaInlineQuery = Class
  private
    FID: String;
    FFrom: TTelegaUser;
    FQuery: String;
    FOffset: String;
  published
    /// <summary>
    /// Unique identifier for this query
    /// </summary>
    [ALIAS('id')]
    property ID: String read FID write FID;
    /// <summary>
    /// Sender
    /// </summary>
    [ALIAS('from')]
    property From: TTelegaUser read FFrom write FFrom;
    [ALIAS('query')]
    /// <summary>
    /// Text of the query
    /// </summary>
    [ALIAS('query')]
    property Query: String read FQuery write FQuery;
    /// <summary>
    /// Offset of the results to be returned, can be controlled by the bot
    /// </summary>
    [ALIAS('offset')]
    property Offset: String read FOffset write FOffset;
  End;

  /// <summary>
  /// This object represents a result of an inline query that was chosen by the user and sent to their chat partner.
  /// </summary>
  TTelegaChosenInlineResult = Class
  private
    FResultId: String;
    FFrom: TTelegaUser;
    FQuery: String;
  published
    /// <summary>
    /// The unique identifier for the result that was chosen.
    /// </summary>
    [ALIAS('result_id')]
    property ResultId: String read FResultId write FResultId;
    /// <summary>
    /// The user that chose the result.
    /// </summary>
    [ALIAS('from')]
    property From: TTelegaUser read FFrom write FFrom;
    /// <summary>
    /// The query that was used to obtain the result.
    /// </summary>
    [ALIAS('query')]
    property Query: String read FQuery write FQuery;
  End;

  TTelegaCallbackQuery = Class
  private
    FID: String;
    FFrom: TTelegaUser;
    FMessage: TTelegaMessage;
    FInlineMessageId: String;
    FData: String;
  published
    /// <summary>
    /// Unique identifier for this query
    /// </summary>
    [ALIAS('id')]
    property ID: String read FID write FID;
    /// <summary>
    /// Sender
    /// </summary>
    [ALIAS('from')]
    property From: TTelegaUser read FFrom write FFrom;
    /// <summary>
    /// Optional. Message with the callback button that originated the query. Note that message content and message date will not be available if the message is too old
    /// </summary>
    [ALIAS('message')]
    property Message: TTelegaMessage read FMessage write FMessage;
    /// <summary>
    /// Optional. Identifier of the message sent via the bot in inline mode, that originated the query
    /// </summary>
    [ALIAS('inline_message_id')]
    property InlineMessageId: String read FInlineMessageId write FInlineMessageId;
    /// <summary>
    /// Data associated with the callback button. Be aware that a bad client can send arbitrary data in this field
    /// </summary>
    [ALIAS('data')]
    property Data: String read FData write FData;
  End;

  /// <summary>
  /// The type of an Update
  /// </summary>

{$SCOPEDENUMS ON} TTelegaUpdateType = (UnkownUpdate = 0, MessageUpdate, InlineQueryUpdate, ChosenInlineResultUpdate, CallbackQueryUpdate); {$SCOPEDENUMS OFF}

  /// <summary>
  /// This object represents an incoming update.
  /// </summary>
  /// <remarks>
  /// Only one of the optional parameters can be present in any given update.
  /// </remarks>
  TTelegaUpdate = Class
  private
    FID: Integer;
    FMessage: TTelegaMessage;
    FInlineQuery: TTelegaInlineQuery;
    FChosenInlineResult: TTelegaChosenInlineResult;
    FCallbackQuery: TTelegaCallbackQuery;
    function Get: TTelegaUpdateType;
  public
    Class Function FromString(Const Data: String): TTelegaUpdate;
  published
    /// <summary>
    /// The update‘s unique identifier. Update identifiers start from a certain positive number and increase sequentially.
    /// This ID becomes especially handy if you’re using Webhooks, since it allows you to ignore repeated updates or to
    /// restore the correct update sequence, should they get out of order.
    /// </summary>
    [ALIAS('update_id')]
    property ID: Integer read FID write FID;
    /// <summary>
    /// Optional. New incoming message of any kind — text, photo, sticker, etc.
    /// </summary>
    [ALIAS('message')]
    property Message: TTelegaMessage read FMessage write FMessage;
    /// <summary>
    /// Optional. New incoming inline query
    /// </summary>
    [ALIAS('inline_query')]
    property InlineQuery: TTelegaInlineQuery read FInlineQuery write FInlineQuery;
    /// <summary>
    /// Optional. The result of a inline query that was chosen by a user and sent to their chat partner
    /// </summary>
    [ALIAS('chosen_inline_result')]
    property ChosenInlineResult: TTelegaChosenInlineResult read FChosenInlineResult
      write FChosenInlineResult;
    /// <summary>
    /// Optional. New incoming callback query
    /// </summary>
    [ALIAS('callback_query')]
    property CallbackQuery: TTelegaCallbackQuery read FCallbackQuery write FCallbackQuery;
    property &Type: TTelegaUpdateType read Get;
  End;

implementation

uses
  System.SysUtils;
{ TTelegaMessage }

function TTelegaMessage.GetType: TTelegaMessageType;
begin
  if Assigned(Audio) then
    Exit(TTelegaMessageType.AudioMessage);
  if Assigned(Document) then
    Exit(TTelegaMessageType.DocumentMessage);
  if Assigned(Photo) then
    Exit(TTelegaMessageType.PhotoMessage);

  if Assigned(Sticker) then
    Exit(TTelegaMessageType.StickerMessage);
  if Assigned(Video) then
    Exit(TTelegaMessageType.VideoMessage);
  if Assigned(Voice) then
    Exit(TTelegaMessageType.VoiceMessage);
  if Assigned(Contact) then
    Exit(TTelegaMessageType.ContactMessage);
  if Assigned(Location) then
    Exit(TTelegaMessageType.LocationMessage);
  if (Text <> '') then
    Exit(TTelegaMessageType.TextMessage);
  if Assigned(Venue) then
    Exit(TTelegaMessageType.VenueMessage);

  if ((NewChatTitle <> '') or Assigned(NewChatPhoto) or Assigned(PinnedMessage) or
    DeleteChatPhoto or GroupChatCreated or SupergroupChatCreated or ChannelChatCreated) then
    Exit(TTelegaMessageType.ServiceMessage);

  Exit(TTelegaMessageType.UnknownMessage);
end;

{ TTelegaApiFileToSend }

constructor TTelegaFileToSend.Create(const FileName: String; const Content: TStream);
begin
  Self.FFileName := FileName;
  Self.FContent := Content;
end;

destructor TTelegaFileToSend.Destroy;
begin
  FContent.Free;
  inherited;
end;

{ TTelegaUpdate }

class function TTelegaUpdate.FromString(const Data: String): TTelegaUpdate;
begin
  Result := Result.FromJSON(Data);
end;

function TTelegaUpdate.Get: TTelegaUpdateType;
begin
  if Assigned(Message) then
    Exit(TTelegaUpdateType.MessageUpdate);
  if Assigned(InlineQuery) then
    Exit(TTelegaUpdateType.InlineQueryUpdate);
  if Assigned(ChosenInlineResult) then
    Exit(TTelegaUpdateType.ChosenInlineResultUpdate);
  if Assigned(CallbackQuery) then
    Exit(TTelegaUpdateType.CallbackQueryUpdate);
end;

end.
