unit TelegAPi.Types;

interface

uses
  System.JSON,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  TelegAPi.Types.Enums,
  DJSON.Attributes,
  TelegAPi.Types.Intf;

type
  TBaseJson = class(TInterfacedObject)
  private
    FJSON: TJSONObject;
  protected
    function ReadToClass<T: class, constructor>(const AKey: string): T;
    function ReadToSimpleType<T>(const AKey: string): T;
    function ReadToDateTime(const AKey: string): TDateTime;
    procedure UnSupported;
  public
    constructor Create(const AJson: string);
    destructor Destroy; override;
  end;

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
    function Status: string;
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
    /// <summary>
    /// The part of the face relative to which the mask should be placed. One
    /// of “forehead”, “eyes”, “mouth”, or “chin”.
    /// </summary>
    [djName('point')]
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
    /// <summary>
    /// List of all set stickers
    /// </summary>
    [djName('stickers')]
    function Stickers: TArray<ItgSticker>;
  end;

  /// <summary>
  /// This object represents a video file.
  /// </summary>
  TtgVideo = class(TtgFile, ItgVideo)
  public
    /// <summary>
    /// Video width as defined by sender
    /// </summary>
    [djName('width')]
    function Width: Int64;
    /// <summary>
    /// Video height as defined by sender
    /// </summary>
    [djName('height')]
    function Height: Int64;
    /// <summary>
    /// Duration of the video in seconds as defined by sender
    /// </summary>
    [djName('duration')]
    function Duration: Int64;
    /// <summary>
    /// Video thumbnail
    /// </summary>
    [djName('thumb')]
    function Thumb: ItgPhotoSize;
    /// <summary>
    /// Optional. Mime type of a file as defined by sender
    /// </summary>
    [djName('mime_type')]
    function MimeType: string;
  end;

  /// <summary>
  /// This object represents a video message
  /// </summary>
  /// <remarks>
  /// available in Telegram apps as of v.4.0
  /// </remarks>
  TtgVideoNote = class(TtgFile, ItgVideoNote)
  public
    /// <summary>
    /// Unique identifier for this file
    /// </summary>
    [djName('file_id')]
    function FileId: string;
    /// <summary>
    /// Video width and height as defined by sender
    /// </summary>
    [djName('length')]
    function Length: Int64;
    /// <summary>
    /// Duration of the video in seconds as defined by sender
    /// </summary>
    [djName('duration')]
    function Duration: Int64;
    /// <summary>
    /// Optional. Video thumbnail
    /// </summary>
    [djName('thumb')]
    function Thumb: ItgPhotoSize;
    /// <summary>
    /// Optional. File size
    /// </summary>
    [djName('file_size')]
    function FileSize: Int64;
  end;

  /// <summary>
  /// This object represents a voice note.
  /// </summary>
  TtgVoice = class(TtgFile, ItgVoice)
  public
    /// <summary>
    /// Duration of the audio in seconds as defined by sender
    /// </summary>
    [djName('duration')]
    function Duration: Int64;
    /// <summary>
    /// Optional. MIME type of the file as defined by sender
    /// </summary>
    [djName('mime_type')]
    function MimeType: string;
  end;

  /// <summary>
  /// This object represents a phone contact.
  /// </summary>
  TtgContact = class(TBaseJson, ItgContact)
  public
    /// <summary>
    /// Contact's phone number
    /// </summary>
    [djName('phone_number')]
    function PhoneNumber: string;
    /// <summary>
    /// Contact's first name
    /// </summary>
    [djName('first_name')]
    function FirstName: string;
    /// <summary>
    /// Optional. Contact's last name
    /// </summary>
    [djName('last_name')]
    function LastName: string;
    /// <summary>
    /// Optional. Contact's user identifier in Telegram
    /// </summary>
    [djName('user_id')]
    function UserId: Int64;
  end;

  TtgLocation = class(TBaseJson, ItgLocation)
  private
    function GetLongitude: Single;
    function GetLatitude: Single;
    procedure SetLatitude(const Value: Single);
    procedure SetLongitude(const Value: Single);
  public
    constructor Create(ALongitude, ALatitude: Single); overload;
    property Longitude: Single read GetLongitude write SetLongitude;
    property Latitude: Single read GetLatitude write SetLatitude;
  end;

  /// <summary>
  /// This object represents a venue.
  /// </summary>
  TtgVenue = class(TBaseJson, ItgVenue)
  public
    /// <summary>
    /// Venue location
    /// </summary>
    [djName('location')]
    function Location: ItgLocation;
    /// <summary>
    /// Title of the result
    /// </summary>
    [djName('title')]
    function Title: string;
    /// <summary>
    /// Address of the venue
    /// </summary>
    [djName('address')]
    function Address: string;
    /// <summary>
    /// Optional. Foursquare identifier of the venue
    /// </summary>
    [djName('foursquare_id')]
    function FoursquareId: string;
  end;

  /// <summary>
  /// You can provide an animation for your game so that it looks stylish in
  /// chats (check out Lumberjack for an example). This object represents an
  /// animation file to be displayed in the message containing a game.
  /// </summary>
  TtgAnimation = class(TBaseJson, ItgAnimation)
  public
    /// <summary>
    /// Unique file identifier
    /// </summary>
    [djName('file_id')]
    function FileId: string;
    /// <summary>
    /// Optional. Animation thumbnail as defined by sender
    /// </summary>
    [djName('thumb')]
    function Thumb: ItgPhotoSize;
    /// <summary>
    /// Optional. Original animation filename as defined by sender
    /// </summary>
    [djName('file_name')]
    function FileName: string;
    /// <summary>
    /// Optional. MIME type of the file as defined by sender
    /// </summary>
    [djName('mime_type')]
    function MimeType: string;
    /// <summary>
    /// Optional. File size
    /// </summary>
    [djName('file_size')]
    function FileSize: Int64;
  end;

  TtgGameHighScore = class(TBaseJson, ItgGameHighScore)
  public
    function Position: Int64;
    function User: ItgUser;
    function Score: Int64;
  end;

  /// <summary>
  /// This object represents a game. Use BotFather to create and edit games,
  /// their short names will act as unique identifiers.
  /// </summary>
  TtgGame = class(TBaseJson, ItgGame)
  public
    /// <summary>
    /// Title of the game
    /// </summary>
    [djName('title')]
    function Title: string;
    /// <summary>
    /// Description of the game
    /// </summary>
    [djName('description')]
    function Description: string;
    /// <summary>
    /// Photo that will be displayed in the game message in chats.
    /// </summary>
    [djName('photo')]
    function Photo: TArray<ItgPhotoSize>;
    /// <summary>
    /// Optional. Brief description of the game or high scores included in
    /// the game message. Can be automatically edited to include current high
    /// scores for the game when the bot calls setGameScore, or manually
    /// edited using editMessageText. 0-4096 characters.
    /// </summary>
    [djName('text')]
    function Text: string;
    /// <summary>
    /// Optional. Special entities that appear in text, such as usernames,
    /// URLs, bot commands, etc.
    /// </summary>
    [djName('text_entities')]
    function TextEntities: TArray<ItgMessageEntity>;
    /// <summary>
    /// Optional. Animation that will be displayed in the game message in
    /// chats. Upload via BotFather
    /// </summary>
    [djName('animation')]
    function Animation: ItgAnimation;
  end;

  /// <summary>
  /// This object represents a message.
  /// </summary>
  TTgMessage = class(TBaseJson, ITgMessage)
  public
    function MessageId: Int64;
    /// <summary>
    /// Sender
    /// </summary>
    [djName('from')]
    function From: ItgUser;
    /// <summary>
    /// Date the message was sent in Unix time
    /// </summary>
    [djName('date')]
    function Date: TDateTime;
    /// <summary>
    /// Conversation the message belongs to
    /// </summary>
    [djName('chat')]
    function Chat: ItgChat;
    /// <summary>
    /// Optional. For forwarded messages, sender of the original message
    /// </summary>
    [djName('forward_from')]
    function ForwardFrom: ItgUser;
    /// <summary>
    /// Optional. For messages forwarded from a channel, information about
    /// the original channel
    /// </summary>
    [djName('forward_from_chat')]
    function ForwardFromChat: ItgChat;
    /// <summary>
    /// Optional. For messages forwarded from channels, identifier of the
    /// original message in the channel
    /// </summary>
    [djName('forward_from_message_id')]
    function ForwardFromMessageId: Int64;
    /// <summary>
    /// Optional. For messages forwarded from channels, signature of the post
    /// author if present
    /// </summary>
    [djName('forward_signature')]
    function ForwardSignature: string;
    /// <summary>
    /// Optional. For forwarded messages, date the original message was sent
    /// in Unix time
    /// </summary>
    [djName('forward_date')]
    function ForwardDate: TDateTime;
    /// <summary>
    /// Optional. For replies, the original message. Note that the Message
    /// object in this field will not contain further reply_to_message fields
    /// even if it itself is a reply.
    /// </summary>
    [djName('reply_to_message')]
    function ReplyToMessage: ITgMessage;
    /// <summary>
    /// Optional. Date the message was last edited in Unix time.
    /// </summary>
    [djName('edit_date')]
    function EditDate: TDateTime;
    /// <summary>
    /// Optional. Signature of the post author for messages in channels
    /// </summary>
    [djName('author_signature')]
    function AuthorSignature: string;
    /// <summary>
    /// Optional. For text messages, the actual UTF-8 text of the message
    /// </summary>
    [djName('text')]
    function Text: string;
    /// <summary>
    /// Optional. For text messages, special entities like usernames, URLs,
    /// bot commands, etc. that appear in the text
    /// </summary>
    [djName('entities')]
    function Entities: TArray<ItgMessageEntity>;
    /// <summary>
    /// Optional. For messages with a caption, special entities like
    /// usernames, URLs, bot commands, etc. that appear in the caption
    /// </summary>
    [djName('caption_entities')]
    function CaptionEntities: TArray<ItgMessageEntity>;
    /// <summary>
    /// Optional. Message is an audio file, information about the file
    /// </summary>
    [djName('audio')]
    function Audio: ItgAudio;
    /// <summary>
    /// Optional. Message is a general file, information about the file
    /// </summary>
    [djName('document')]
    function Document: ItgDocument;
    /// <summary>
    /// Optional. Message is a game, information about the game.
    /// </summary>
    [djName('game')]
    function Game: ItgGame;
    /// <summary>
    /// Optional. Message is a photo, available sizes of the photo
    /// </summary>
    [djName('photo')]
    function Photo: TArray<ItgPhotoSize>;
    /// <summary>
    /// Optional. Message is a sticker, information about the sticker
    /// </summary>
    [djName('sticker')]
    function Sticker: ItgSticker;
    /// <summary>
    /// Optional. Message is a video, information about the video
    /// </summary>
    [djName('video')]
    function Video: ItgVideo;
    /// <summary>
    /// Message is a voice message, information about the file
    /// </summary>
    [djName('voice')]
    function Voice: ItgVoice;
    /// <summary>
    /// Optional. Message is a video note, information about the video
    /// message
    /// </summary>
    [djName('video_note')]
    function VideoNote: ItgVideoNote;
    /// <summary>
    /// Optional. New members that were added to the group or supergroup and
    /// information about them (the bot itself may be one of these members)
    /// </summary>
    [djName('new_chat_members')]
    function NewChatMembers: TArray<ItgUser>;
    /// <summary>
    /// Optional. Caption for the document, photo or video, 0-200 characters
    /// </summary>
    [djName('caption')]
    function Caption: string;
    /// <summary>
    /// Optional. Message is a shared contact, information about the contact
    /// </summary>
    [djName('contact')]
    function Contact: ItgContact;
    /// <summary>
    /// Optional. Message is a shared location, information about the
    /// location
    /// </summary>
    [djName('location')]
    function Location: ItgLocation;
    /// <summary>
    /// Optional. Message is a venue, information about the venue
    /// </summary>
    [djName('venue')]
    function Venue: ItgVenue;
    /// <summary>
    /// Optional. A new member was added to the group, information about them
    /// (this member may be the bot itself)
    /// </summary>
    [djName('new_chat_member')]
    function NewChatMember: ItgUser;
    /// <summary>
    /// Optional. A member was removed from the group, information about them
    /// (this member may be bot itself)
    /// </summary>
    [djName('left_chat_member')]
    function LeftChatMember: ItgUser;
    /// <summary>
    /// Optional. A group title was changed to this value
    /// </summary>
    [djName('new_chat_title')]
    function NewChatTitle: string;
    /// <summary>
    /// Optional. A group photo was change to this value
    /// </summary>
    [djName('new_chat_photo')]
    function NewChatPhoto: TArray<ItgPhotoSize>;
    /// <summary>
    /// Optional. Informs that the group photo was deleted
    /// </summary>
    [djName('delete_chat_photo')]
    function DeleteChatPhoto: Boolean;
    /// <summary>
    /// Optional. Informs that the group has been created
    /// </summary>
    [djName('group_chat_created')]
    function GroupChatCreated: Boolean;
    /// <summary>
    /// Optional. Service message: the supergroup has been created
    /// </summary>
    [djName('supergroup_chat_created')]
    function SupergroupChatCreated: Boolean;
    /// <summary>
    /// Optional. Service message: the channel has been created
    /// </summary>
    [djName('channel_chat_created')]
    function ChannelChatCreated: Boolean;
    /// <summary>
    /// Optional. The group has been migrated to a supergroup with the
    /// specified identifier
    /// </summary>
    [djName('migrate_to_chat_id')]
    function MigrateToChatId: Int64;
    /// <summary>
    /// Optional. The supergroup has been migrated from a group with the
    /// specified identifier
    /// </summary>
    [djName('migrate_from_chat_id')]
    function MigrateFromChatId: Int64;
    /// <summary>
    /// Optional. Specified message was pinned. Note that the Message object
    /// in this field will not contain further reply_to_message fields even
    /// if it is itself a reply
    /// </summary>
    [djName('pinned_message')]
    function PinnedMessage: ITgMessage;
    /// <summary>
    /// Optional. Message is an invoice for a <see href="https://core.telegram.org/bots/api#payments">
    /// payment</see>, information about the invoice. <see href="https://core.telegram.org/bots/api#payments">
    /// More about payments »</see>
    /// </summary>
    [djName('invoice')]
    function Invoice: ItgInvoice;
    /// <summary>
    /// Optional. Message is a service message about a successful payment,
    /// information about the payment. <see href="https://core.telegram.org/bots/api#payments">
    /// More about payments »</see>
    /// </summary>
    [djName('successful_payment')]
    function SuccessfulPayment: ItgSuccessfulPayment;
    function &type: TtgMessageType;
  end;

  /// <summary>
  /// This object represent a user's profile pictures.
  /// </summary>
  [djName('UserProfilePhotos')]
  TtgUserProfilePhotos = class
  public
    /// <summary>
    /// Total number of profile pictures the target user has
    /// </summary>
    [djName('total_count')]
    TotalCount: Int64;
    /// <summary>
    /// Requested profile pictures (in up to 4 sizes each)
    /// </summary>
    [djName('photos')]
    Photos: TObjectList<TObjectList<TtgPhotoSize>>;
    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  /// A placeholder, currently holds no information.
  /// </summary>

  TtgCallbackGame = class
  end;

  /// <summary>
  /// Contains information about why a request was unsuccessfull.
  /// </summary>
  TtgResponseParameters = class(TBaseJson, ItgResponseParameters)
  public
    function MigrateToChatId: Int64;
    function RetryAfter: Int64;
  end;

  [djName('FileToSend')]
  TtgFileToSend = class
  public
    FileName: string;
    Content: TStream;
    constructor Create(const AFileName: string); overload;
    constructor Create(AContent: TStream;
      const AFileName: string = ''); overload;
  end;

  /// <summary>
  /// This object represents an incoming inline query. When the user sends an
  /// empty query, your bot could return some default or trending results.
  /// </summary>
  [djName('InlineQuery')]
  TtgInlineQuery = class(TBaseJson, ItgInlineQuery)
  public
    /// <summary>
    /// Unique identifier for this query
    /// </summary>
    [djName('id')]
    function ID: string;
    /// <summary>
    /// Sender
    /// </summary>
    [djName('from')]
    function From: ItgUser;
    /// <summary>
    /// Text of the query
    /// </summary>
    [djName('query')]
    function Query: string;
    /// <summary>
    /// Offset of the results to be returned, can be controlled by the bot
    /// </summary>
    [djName('offset')]
    function Offset: string;
  end;

  /// <summary>
  /// Represents a result of an inline query that was chosen by the user and
  /// sent to their chat partner.
  /// </summary>
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

  /// <summary>
  /// This object represents a portion of the price for goods or services.
  /// </summary>
  [djName('LabeledPrice')]
  TtgLabeledPrice = class
  public
    /// <summary>
    /// Portion label
    /// </summary>
    [djName('label')]
    Text: string;
    /// <summary>
    /// Price of the product in the smallest units of the <see href="https://core.telegram.org/bots/payments#supported-currencies">
    /// currency</see> (Int64, not float/double).
    /// </summary>
    /// <example>
    /// For example, for a price of <c>US$ 1.45</c> pass <c>amount = 145</c>
    /// . See the exp parameter in <see href="https://core.telegram.org/bots/payments/currencies.json">
    /// currencies.json</see>, it shows the number of digits past the decimal
    /// point for each <br />currency (2 for the majority of currencies). <br />
    /// </example>
    [djName('amount')]
    Amount: Int64;
    constructor Create; overload;
    constructor Create(const AText: string; AAmount: Int64); overload;
  end;

  /// <summary>
  /// This object represents a shipping address.
  /// </summary>
  TtgShippingAddress = class(TBaseJson, ItgShippingAddress)
  public
    /// <summary>
    /// ISO 3166-1 alpha-2 country code
    /// </summary>
    [djName('country_code')]
    function CountryCode: string;
    /// <summary>
    /// State, if applicable
    /// </summary>
    [djName('state')]
    function State: string;
    /// <summary>
    /// City
    /// </summary>
    [djName('city')]
    function City: string;
    /// <summary>
    /// First line for the address
    /// </summary>
    [djName('street_line1')]
    function StreetLine1: string;
    /// <summary>
    /// Second line for the address
    /// </summary>
    [djName('street_line2')]
    function StreetLine2: string;
    /// <summary>
    /// Address post code
    /// </summary>
    [djName('post_code')]
    function PostCode: string;
  end;

  /// <summary>
  /// This object represents information about an order.
  /// </summary>
  TtgOrderInfo = class(TBaseJson, ItgOrderInfo)
  public
    /// <summary>
    /// Optional. User name
    /// </summary>
    [djName('name')]
    function Name: string;
    /// <summary>
    /// Optional. User's phone number
    /// </summary>
    [djName('phone_number')]
    function PhoneNumber: string;
    /// <summary>
    /// Optional. User email
    /// </summary>
    [djName('email')]
    function Email: string;
    /// <summary>
    /// Optional. User shipping address
    /// </summary>
    [djName('shipping_address')]
    function ShippingAddress: ItgShippingAddress;
  end;

  /// <summary>
  /// This object contains information about an incoming pre-checkout query.
  /// </summary>
  [djName('PreCheckoutQuery')]
  TtgPreCheckoutQuery = class(TBaseJson, ItgPreCheckoutQuery)
  public
    /// <summary>
    /// Unique query identifier
    /// </summary>
    [djName('id')]
    function ID: string;
    /// <summary>
    /// User who sent the query
    /// </summary>
    [djName('from')]
    function From: ItgUser;
    /// <summary>
    /// Three-letter ISO 4217 <see href="https://core.telegram.org/bots/payments#supported-currencies">
    /// currency</see> code
    /// </summary>
    [djName('currency')]
    function Currency: string;
    /// <summary>
    /// Total price in the smallest units of the currency (Int64, not
    /// float/double). For example, for a price of <c>US$ 1.45</c> pass <c>
    /// amount = 145</c>. See the <c>exp</c> parameter in <see href="https://core.telegram.org/bots/payments/currencies.json">
    /// currencies.json</see>, it shows the number of digits past the decimal
    /// point for each currency (2 for the majority of currencies).
    /// </summary>
    [djName('total_amount')]
    function TotalAmount: Int64;
    /// <summary>
    /// Bot specified invoice payload
    /// </summary>
    [djName('invoice_payload')]
    function InvoicePayload: string;
    /// <summary>
    /// Optional. Identifier of the shipping option chosen by the user
    /// </summary>
    [djName('shipping_option_id')]
    function ShippingOptionId: string;
    /// <summary>
    /// Optional. Order info provided by the user
    /// </summary>
    [djName('order_info')]
    function OrderInfo: ItgOrderInfo;
  end;

  /// <summary>
  /// This object represents one shipping option.
  /// </summary>
  TtgShippingOption = class(TBaseJson, ItgShippingOption)
  public
    /// <summary>
    /// Shipping option identifier
    /// </summary>
    [djName('id')]
    function ID: string;
    /// <summary>
    /// Option title
    /// </summary>
    [djName('title')]
    function Title: string;
    /// <summary>
    /// List of price portions
    /// </summary>
    [djName('prices')]
    function Prices: TArray<ItgLabeledPrice>;
  end;

  /// <summary>
  /// This object contains information about an incoming shipping query.
  /// </summary>
  TtgShippingQuery = class(TBaseJson, ItgShippingQuery)
  public
    /// <summary>
    /// Unique query identifier
    /// </summary>
    [djName('id')]
    function ID: string;
    /// <summary>
    /// User who sent the query
    /// </summary>
    [djName('from')]
    function From: ItgUser;
    /// <summary>
    /// Bot specified invoice payload
    /// </summary>
    [djName('invoice_payload')]
    function InvoicePayload: string;
    /// <summary>
    /// User specified shipping address
    /// </summary>
    [djName('shipping_address')]
    function ShippingAddress: ItgShippingAddress;
  end;

  /// <summary>
  /// This object contains basic information about a successful payment.
  /// </summary>
  TtgSuccessfulPayment = class(TBaseJson, ItgSuccessfulPayment)
  public
    /// <summary>
    /// Three-letter ISO 4217 <see href="https://core.telegram.org/bots/payments#supported-currencies">
    /// currency</see> code
    /// </summary>
    [djName('currency')]
    function Currency: string;
    /// <summary>
    /// Total price in the smallest units of the currency (Int64, not
    /// float/double). For example, for a price of <c>US$ 1.45</c> pass <c>
    /// amount = 145</c>. See the <c>exp</c> parameter in <see href="https://core.telegram.org/bots/payments/currencies.json">
    /// currencies.json</see>, it shows the number of digits past the decimal
    /// point for each currency (2 for the majority of currencies).
    /// </summary>
    [djName('total_amount')]
    function TotalAmount: Int64;
    /// <summary>
    /// Bot specified invoice payload
    /// </summary>
    [djName('invoice_payload')]
    function InvoicePayload: string;
    /// <summary>
    /// Optional. Identifier of the shipping option chosen by the user
    /// </summary>
    [djName('shipping_option_id')]
    function ShippingOptionId: string;
    /// <summary>
    /// Optional. Order info provided by the user
    /// </summary>
    [djName('order_info')]
    function OrderInfo: ItgOrderInfo;
    /// <summary>
    /// Telegram payment identifier
    /// </summary>
    [djName('telegram_payment_charge_id')]
    function TelegramPaymentChargeId: string;
    /// <summary>
    /// Provider payment identifier
    /// </summary>
    [djName('provider_payment_charge_id')]
    function ProviderPaymentChargeId: string;
  end;
{$ENDREGION}
  /// <summary>
  /// This object represents an incoming update.
  /// </summary>
  /// <remarks>
  /// Only one of the optional parameters can be present in any given update.
  /// </remarks>

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
    function &type: TtgUpdateType;
  end;

  /// <summary>
  /// Contains information about the current status of a webhook.
  /// </summary>
  TtgWebhookInfo = class(TBaseJson, ItgWebhookInfo)
  public
    /// <summary>
    /// Webhook URL, may be empty if webhook is not set up
    /// </summary>
    [djName('url')]
    function Url: string;
    /// <summary>
    /// True, if a custom certificate was provided for webhook certificate
    /// checks
    /// </summary>
    [djName('has_custom_certificate')]
    function HasCustomCertificate: Boolean;
    /// <summary>
    /// Number of updates awaiting delivery
    /// </summary>
    [djName('pending_update_count')]
    function PendingUpdateCount: Int64;
    /// <summary>
    /// Optional. Unix time for the most recent error that happened when
    /// trying to deliver an update via webhook
    /// </summary>
    [djName('last_error_date')]
    function LastErrorDate: TDateTime;
    /// <summary>
    /// Optional. Error message in human-readable format for the most recent
    /// error that happened when trying to deliver an update via webhook
    /// </summary>
    [djName('last_error_message')]
    function LastErrorMessage: string;
    /// <summary>
    /// Optional. Maximum allowed number of simultaneous HTTPS connections to
    /// the webhook for update delivery
    /// </summary>
    [djName('max_connections')]
    function MaxConnections: Int64;
    /// <summary>
    /// Optional. A list of update types the bot is subscribed to. Defaults
    /// to all update types
    /// </summary>
    [djName('allowed_updates')]
    function AllowedUpdates: TArray<string>;
  end;

implementation

uses
  FMX.Types,
  System.DateUtils,
  System.TypInfo,
  System.Rtti;
{ TtgAnimation }

function TtgAnimation.FileId: string;
begin
  FJSON.TryGetValue<string>('data', Result);
end;

function TtgAnimation.FileName: string;
begin
  FJSON.TryGetValue<string>('data', Result);
end;

function TtgAnimation.FileSize: Int64;
begin
  FJSON.TryGetValue<Int64>('file_size', Result);
end;

function TtgAnimation.MimeType: string;
begin
  FJSON.TryGetValue<string>('mime_type', Result);
end;

function TtgAnimation.Thumb: ItgPhotoSize;
begin
  Result := ReadToClass<TtgPhotoSize>('thumb');
end;

{ TtgCallbackQuery }
function TtgCallbackQuery.Data: string;
begin
  FJSON.TryGetValue<string>('data', Result);
end;

function TtgCallbackQuery.From: ItgUser;
begin
  Result := ReadToClass<TtgUser>('from');
end;

function TtgCallbackQuery.GameShortName: string;
begin
  FJSON.TryGetValue<string>('game_short_name', Result);
end;

function TtgCallbackQuery.ID: string;
begin
  FJSON.TryGetValue<string>('id', Result);
end;

function TtgCallbackQuery.InlineMessageId: string;
begin
  FJSON.TryGetValue<string>('inline_message_id', Result);
end;

function TtgCallbackQuery.Message: ITgMessage;
begin
  Result := ReadToClass<TTgMessage>('message');
end;

{ TtgDocument }

function TtgDocument.FileName: string;
begin
  FJSON.TryGetValue<string>('file_name', Result);
end;

function TtgDocument.MimeType: string;
begin
  FJSON.TryGetValue<string>('mime_type', Result);
end;

function TtgDocument.Thumb: ItgPhotoSize;
begin
  Result := ReadToClass<TtgPhotoSize>('thumb');
end;

{ TtgFile }
function TtgFile.CanDownload: Boolean;
begin
  Result := not FilePath.IsEmpty;
end;

function TtgFile.FileId: string;
begin
  FJSON.TryGetValue<string>('file_id', Result);
end;

function TtgFile.FilePath: string;
begin
  FJSON.TryGetValue<string>('file_path', Result);
end;

function TtgFile.FileSize: Int64;
begin
  FJSON.TryGetValue<Int64>('file_size', Result);
end;

function TtgFile.GetFileUrl(const AToken: string): string;
begin
  Result := 'https://api.telegram.org/file/bot' + AToken + '/' + FilePath;
end;

{ TtgFileToSend }
constructor TtgFileToSend.Create(const AFileName: string);
begin
  Content := nil;
  FileName := AFileName;
  if not FileExists(AFileName) then
    raise EFileNotFoundException.CreateFmt('File %S not found!', [AFileName]);
end;

constructor TtgFileToSend.Create(AContent: TStream; const AFileName: string);
begin
  FileName := AFileName;
  Content := AContent;
  // I guess, in most cases, AFilename param should contain a non-empty string.
  // It is odd to receive a file with filename and
  // extension which both are not connected with its content.
  if AFileName.IsEmpty then
    raise Exception.Create('TtgFileToSend: Filename is empty!');
  if not Assigned(AContent) then
    raise EStreamError.Create('Stream not assigned!');
  FileName := AFileName;
  Content := AContent;
end;

function TtgGameHighScore.Position: Int64;
begin
  FJSON.TryGetValue<Int64>('position', Result);
end;

function TtgGameHighScore.Score: Int64;
begin
  FJSON.TryGetValue<Int64>('score', Result);
end;

function TtgGameHighScore.User: ItgUser;
begin
  Result := ReadToClass<TtgUser>('user');
end;

{ TtgMessage }

function TTgMessage.Document: ItgDocument;
begin
  Result := ReadToClass<TtgDocument>('document');
end;

function TTgMessage.EditDate: TDateTime;
begin
  Result := ReadToDateTime('edit_date');
end;

function TTgMessage.Entities: TArray<ItgMessageEntity>;
var
  LValue: string;
  LJsonArray: TJSONArray;
  I: Integer;
begin
  if FJSON.TryGetValue<string>('entities', LValue) then
  begin
    LJsonArray := TJSONObject.ParseJSONValue(LValue) as TJSONArray;
    try
      SetLength(Result, LJsonArray.Count);
      for I := 0 to LJsonArray.Count - 1 do
        Result[I] := ReadToClass<TtgMessageEntity>('entities');
    finally
      LJsonArray.Free;
    end;
  end;
end;

function TTgMessage.ForwardDate: TDateTime;
begin
  Result := ReadToDateTime('forward_date');
end;

function TTgMessage.ForwardFrom: ItgUser;
begin
  Result := ReadToClass<TtgUser>('forward_from');
end;

function TTgMessage.ForwardFromChat: ItgChat;
begin
  Result := ReadToClass<TtgChat>('forward_from_chat');
end;

function TTgMessage.ForwardFromMessageId: Int64;
begin
  FJSON.TryGetValue<Int64>('forward_from_message_id', Result);
end;

function TTgMessage.ForwardSignature: string;
begin
  FJSON.TryGetValue<string>('forward_signature', Result);
end;

function TTgMessage.From: ItgUser;
begin
  Result := ReadToClass<TtgUser>('from');
end;

function TTgMessage.Game: ItgGame;
begin
  Result := ReadToClass<TtgGame>('game');
end;

function TTgMessage.GroupChatCreated: Boolean;
begin
  FJSON.TryGetValue<Boolean>('group_chat_created', Result);
end;

function TTgMessage.Invoice: ItgInvoice;
begin
  Result := ReadToClass<TtgInvoice>('invoice');
end;

function TTgMessage.LeftChatMember: ItgUser;
begin
  Result := ReadToClass<TtgUser>('left_chat_member');
end;

function TTgMessage.Location: ItgLocation;
begin
  Result := ReadToClass<TtgLocation>('location');
end;

function TTgMessage.MessageId: Int64;
begin
  FJSON.TryGetValue<Int64>('message_id', Result);
end;

function TTgMessage.MigrateFromChatId: Int64;
begin
  FJSON.TryGetValue<Int64>('migrate_from_chat_id', Result);
end;

function TTgMessage.MigrateToChatId: Int64;
begin
  FJSON.TryGetValue<Int64>('migrate_to_chat_id', Result);
end;

function TTgMessage.NewChatMember: ItgUser;
begin
  Result := ReadToClass<TtgUser>('new_chat_member');
end;

function TTgMessage.NewChatMembers: TArray<ItgUser>;
begin
  UnSupported;
end;

function TTgMessage.NewChatPhoto: TArray<ItgPhotoSize>;
begin
  UnSupported;
end;

function TTgMessage.NewChatTitle: string;
begin
  FJSON.TryGetValue<string>('new_chat_title', Result);
end;

function TTgMessage.Photo: TArray<ItgPhotoSize>;
begin
  UnSupported;
end;

function TTgMessage.PinnedMessage: ITgMessage;
begin
  Result := ReadToClass<TTgMessage>('pinned_message');
end;

function TTgMessage.ReplyToMessage: ITgMessage;
begin
  Result := ReadToClass<TTgMessage>('reply_to_message');
end;

function TTgMessage.Sticker: ItgSticker;
begin
  Result := ReadToClass<TtgSticker>('sticker');
end;

function TTgMessage.SuccessfulPayment: ItgSuccessfulPayment;
begin
  Result := ReadToClass<TtgSuccessfulPayment>('successful_payment');
end;

function TTgMessage.SupergroupChatCreated: Boolean;
begin
  FJSON.TryGetValue<Boolean>('supergroup_chat_created', Result);
end;

function TTgMessage.Text: string;
begin
  Result := ReadToSimpleType<string>('text');
end;

function TTgMessage.&type: TtgMessageType;
begin
  if Audio <> nil then
    Exit(TtgMessageType.AudioMessage);
  if Contact <> nil then
    Exit(TtgMessageType.ContactMessage);
  if Document <> nil then
    Exit(TtgMessageType.DocumentMessage);
  if Game <> nil then
    Exit(TtgMessageType.GameMessage);
  if (Location <> nil) then
    Exit(TtgMessageType.LocationMessage);
  if (NewChatMember <> nil) or (LeftChatMember <> nil) or
    ((NewChatPhoto <> nil) and (Length(NewChatPhoto) > 0)) or
    ((NewChatMembers <> nil) and (Length(NewChatMembers) > 0)) or
    (not NewChatTitle.IsEmpty) or DeleteChatPhoto or GroupChatCreated or
    SupergroupChatCreated or ChannelChatCreated or (MigrateToChatId <> 0) or
    (MigrateFromChatId <> 0) or (PinnedMessage <> nil) then
    Exit(TtgMessageType.ServiceMessage);
  if (Photo <> nil) and (Length(Photo) > 0) then
    Exit(TtgMessageType.PhotoMessage);
  if (Sticker <> nil) then
    Exit(TtgMessageType.StickerMessage);
  if (Venue <> nil) then
    Exit(TtgMessageType.VenueMessage);
  if (Video <> nil) then
    Exit(TtgMessageType.VideoMessage);
  if (VideoNote <> nil) then
    Exit(TtgMessageType.VideoNoteMessage);
  if (Voice <> nil) then
    Exit(TtgMessageType.VoiceMessage);
  if not Text.IsEmpty then
    Exit(TtgMessageType.TextMessage);
  Result := TtgMessageType.UnknownMessage;
end;

function TTgMessage.Audio: ItgAudio;
begin
  Result := ReadToClass<TtgAudio>('audio');
end;

function TTgMessage.AuthorSignature: string;
begin
  FJSON.TryGetValue<string>('author_signature', Result);
end;

function TTgMessage.Caption: string;
begin
  FJSON.TryGetValue<string>('caption', Result);
end;

function TTgMessage.CaptionEntities: TArray<ItgMessageEntity>;
begin
  UnSupported;
end;

function TTgMessage.ChannelChatCreated: Boolean;
begin
  FJSON.TryGetValue<Boolean>('channel_chat_created', Result);
end;

function TTgMessage.Chat: ItgChat;
begin
  Result := ReadToClass<TtgChat>('chat');
end;

function TTgMessage.Contact: ItgContact;
begin
  Result := ReadToClass<TtgContact>('contact');
end;

function TTgMessage.Date: TDateTime;
begin
  Result := ReadToDateTime('date');
end;

function TTgMessage.DeleteChatPhoto: Boolean;
begin
  FJSON.TryGetValue<Boolean>('delete_chat_photo', Result);
end;

function TTgMessage.Venue: ItgVenue;
begin
  Result := ReadToClass<TtgVenue>('venue');
end;

function TTgMessage.Video: ItgVideo;
begin
  Result := ReadToClass<TtgVideo>('video');
end;

function TTgMessage.VideoNote: ItgVideoNote;
begin
  Result := ReadToClass<TtgVideoNote>('video_note');
end;

function TTgMessage.Voice: ItgVoice;
begin
  Result := ReadToClass<TtgVoice>('voice');
end;

{ TtgShippingOption }

function TtgShippingOption.ID: string;
begin
  FJSON.TryGetValue<string>('id', Result);
end;

function TtgShippingOption.Prices: TArray<ItgLabeledPrice>;
begin
  UnSupported;
end;

function TtgShippingOption.Title: string;
begin
  FJSON.TryGetValue<string>('title', Result);
end;

{ TtgUpdate }
function TtgUpdate.CallbackQuery: ItgCallbackQuery;
begin
  Result := ReadToClass<TtgCallbackQuery>('callback_query');
end;

function TtgUpdate.ChannelPost: ITgMessage;
begin
  Result := ReadToClass<TTgMessage>('channel_post');
end;

function TtgUpdate.ChosenInlineResult: ItgChosenInlineResult;
begin
  Result := ReadToClass<TtgChosenInlineResult>('chosen_inline_result');
end;

function TtgUpdate.EditedChannelPost: ITgMessage;
begin
  Result := ReadToClass<TTgMessage>('edited_channel_post');
end;

function TtgUpdate.EditedMessage: ITgMessage;
begin
  Result := ReadToClass<TTgMessage>('edited_message');
end;

function TtgUpdate.ID: Int64;
begin
  FJSON.TryGetValue<Int64>('id', Result);
end;

function TtgUpdate.InlineQuery: ItgInlineQuery;
begin
  Result := ReadToClass<TtgInlineQuery>('inline_query');
end;

function TtgUpdate.Message: ITgMessage;
begin
  Result := ReadToClass<TTgMessage>('message');
end;

function TtgUpdate.PreCheckoutQuery: ItgPreCheckoutQuery;
begin
  Result := ReadToClass<TtgPreCheckoutQuery>('pre_checkout_query');
end;

function TtgUpdate.ShippingQuery: ItgShippingQuery;
begin
  Result := ReadToClass<TtgShippingQuery>('shipping_query');
end;

function TtgUpdate.&type: TtgUpdateType;
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

{ TtgUserProfilePhotos }
constructor TtgUserProfilePhotos.Create;
begin
  Photos := TObjectList < TObjectList < TtgPhotoSize >>.Create();
end;

destructor TtgUserProfilePhotos.Destroy;
begin
  Photos.Free;
  inherited;
end;

{ TtgLocation }

constructor TtgLocation.Create(ALongitude, ALatitude: Single);
begin
  SetLongitude(ALongitude);
  SetLatitude(ALatitude);
end;

function TtgLocation.GetLatitude: Single;
begin
  FJSON.TryGetValue<Single>('latitude', Result);
end;

function TtgLocation.GetLongitude: Single;
begin
  FJSON.TryGetValue<Single>('longitude', Result);
end;

procedure TtgLocation.SetLatitude(const Value: Single);
begin
  FJSON.AddPair('latitude', TJSONNumber.Create(Value));
end;

procedure TtgLocation.SetLongitude(const Value: Single);
begin
  FJSON.AddPair('longitude', TJSONNumber.Create(Value));
end;

{ TtgStickerSet }

function TtgStickerSet.ContainsMasks: Boolean;
begin
  FJSON.TryGetValue<Boolean>('contains_masks', Result);
end;

function TtgStickerSet.Name: string;
begin
  FJSON.TryGetValue<string>('name', Result);
end;

function TtgStickerSet.Stickers: TArray<ItgSticker>;
begin
  UnSupported;
end;

function TtgStickerSet.Title: string;
begin
  FJSON.TryGetValue<string>('title', Result);
end;

{ TtgLabeledPrice }

constructor TtgLabeledPrice.Create;
begin
  inherited Create;
end;

constructor TtgLabeledPrice.Create(const AText: string; AAmount: Int64);
begin
  Text := AText;
  Amount := AAmount;
end;

{ TBaseJson }

constructor TBaseJson.Create(const AJson: string);
begin
  FJSON := TJSONObject.ParseJSONValue(AJson) as TJSONObject;
  Log.d(FJSON.ToJSON)
end;

function TBaseJson.ReadToClass<T>(const AKey: string): T;
var
  LValue: string;
  LObj: TJSONValue;
begin
  Log.d('open "%S" to @%S', [AKey, gettypeName(TypeInfo(T))]);
  LObj := FJSON.GetValue(AKey);
  try
    if Assigned(LObj) then
    begin
      LValue := LObj.ToJSON;
      Result := T(TBaseJson.Create(LValue));
    end
    else
      Result := nil;
  finally
    // LObj.Free;
  end;
end;

destructor TBaseJson.Destroy;
begin
  FJSON.Free;
  inherited;
end;

function TBaseJson.ReadToDateTime(const AKey: string): TDateTime;
var
  LValue: Int64;
begin
  if FJSON.TryGetValue<Int64>(AKey, LValue) then
    Result := UnixToDateTime(LValue, False);
end;

function TBaseJson.ReadToSimpleType<T>(const AKey: string): T;
begin
  Log.d('open "%S" to @%S', [AKey, gettypeName(TypeInfo(T))]);
  FJSON.TryGetValue<T>(AKey, Result);
end;

procedure TBaseJson.UnSupported;
begin
  raise Exception.Create
    ('Telegram method not supported in TelegaPi Library. Sorry.');
end;

{ TtgResponseParameters }

function TtgResponseParameters.MigrateToChatId: Int64;
begin
  FJSON.TryGetValue<Int64>('migrate_to_chat_id', Result);
end;

function TtgResponseParameters.RetryAfter: Int64;
begin
  FJSON.TryGetValue<Int64>('retry_after', Result);
end;

{ TtgUser }

function TtgUser.FirstName: string;
begin
  Result := FJSON.GetValue<string>('first_name');
end;

function TtgUser.ID: Int64;
begin
  Result := FJSON.GetValue<Int64>('id');
end;

function TtgUser.IsBot: Boolean;
begin
  Result := FJSON.GetValue<Boolean>('is_bot');
end;

function TtgUser.LanguageCode: string;
begin
  Result := FJSON.GetValue<string>('language_code');
end;

function TtgUser.LastName: string;
begin
  Result := FJSON.GetValue<string>('last_name');
end;

function TtgUser.Username: string;
begin
  Result := FJSON.GetValue<string>('username');
end;

{ TtgInlineQuery }

function TtgInlineQuery.From: ItgUser;
begin
  Result := ReadToClass<TtgUser>('from');
end;

function TtgInlineQuery.ID: string;
begin
  Result := FJSON.GetValue<string>('id');
end;

function TtgInlineQuery.Offset: string;
begin
  Result := FJSON.GetValue<string>('offset');
end;

function TtgInlineQuery.Query: string;
begin
  Result := FJSON.GetValue<string>('query');
end;

{ TtgChosenInlineResult }

function TtgChosenInlineResult.From: ItgUser;
begin
  Result := ReadToClass<TtgUser>('from');
end;

function TtgChosenInlineResult.InlineMessageId: string;
begin
  Result := FJSON.GetValue<string>('inline_message_id');
end;

function TtgChosenInlineResult.Location: ItgLocation;
begin
  Result := ReadToClass<TtgLocation>('location');
end;

function TtgChosenInlineResult.Query: string;
begin
  Result := FJSON.GetValue<string>('query');
end;

function TtgChosenInlineResult.ResultId: string;
begin
  Result := FJSON.GetValue<string>('result_id');
end;

{ TtgPreCheckoutQuery }

function TtgPreCheckoutQuery.Currency: string;
begin
  Result := FJSON.GetValue<string>('currency');
end;

function TtgPreCheckoutQuery.From: ItgUser;
begin
  Result := ReadToClass<TtgUser>('from');
end;

function TtgPreCheckoutQuery.ID: string;
begin
  Result := FJSON.GetValue<string>('id');
end;

function TtgPreCheckoutQuery.InvoicePayload: string;
begin
  Result := FJSON.GetValue<string>('invoice_payload');
end;

function TtgPreCheckoutQuery.OrderInfo: ItgOrderInfo;
begin
  Result := ReadToClass<TtgOrderInfo>('from');
end;

function TtgPreCheckoutQuery.ShippingOptionId: string;
begin
  Result := FJSON.GetValue<string>('shipping_option_id');
end;

function TtgPreCheckoutQuery.TotalAmount: Int64;
begin
  Result := FJSON.GetValue<Int64>('total_amount');
end;

{ TtgShippingQuery }

function TtgShippingQuery.From: ItgUser;
begin
  Result := ReadToClass<TtgUser>('from');
end;

function TtgShippingQuery.ID: string;
begin
  Result := FJSON.GetValue<string>('id');
end;

function TtgShippingQuery.InvoicePayload: string;
begin
  Result := FJSON.GetValue<string>('invoice_payload');
end;

function TtgShippingQuery.ShippingAddress: ItgShippingAddress;
begin
  Result := ReadToClass<TtgShippingAddress>('shipping_address');
end;

{ TtgChatPhoto }

function TtgChatPhoto.BigFileId: string;
begin
  FJSON.TryGetValue<string>('big_file_id', Result);
end;

function TtgChatPhoto.SmallFileId: string;
begin
  FJSON.TryGetValue<string>('small_file_id', Result);
end;

{ TtgChatMember }

function TtgChatMember.CanAddWebPagePreviews: Boolean;
begin
  FJSON.TryGetValue<Boolean>('can_add_web_page_previews', Result);
end;

function TtgChatMember.CanBeEdited: Boolean;
begin
  FJSON.TryGetValue<Boolean>('can_change_info', Result);
end;

function TtgChatMember.CanChangeInfo: Boolean;
begin
  FJSON.TryGetValue<Boolean>('can_change_info', Result);
end;

function TtgChatMember.CanDeleteMessages: Boolean;
begin
  FJSON.TryGetValue<Boolean>('can_delete_messages', Result);
end;

function TtgChatMember.CanEditMessages: Boolean;
begin
  FJSON.TryGetValue<Boolean>('can_edit_messages', Result);
end;

function TtgChatMember.CanInviteUsers: Boolean;
begin
  FJSON.TryGetValue<Boolean>('can_invite_users', Result);
end;

function TtgChatMember.CanPinMessages: Boolean;
begin
  FJSON.TryGetValue<Boolean>('can_pin_messages', Result);
end;

function TtgChatMember.CanPostMessages: Boolean;
begin
  FJSON.TryGetValue<Boolean>('can_post_messages', Result);
end;

function TtgChatMember.CanPromoteMembers: Boolean;
begin
  FJSON.TryGetValue<Boolean>('can_promote_members', Result);
end;

function TtgChatMember.CanRestrictMembers: Boolean;
begin
  FJSON.TryGetValue<Boolean>('can_restrict_members', Result);
end;

function TtgChatMember.CanSendMediaMessages: Boolean;
begin
  FJSON.TryGetValue<Boolean>('can_send_media_messages', Result);
end;

function TtgChatMember.CanSendMessages: Boolean;
begin
  FJSON.TryGetValue<Boolean>('can_send_messages', Result);
end;

function TtgChatMember.CanSendOtherMessages: Boolean;
begin
  FJSON.TryGetValue<Boolean>('can_send_other_messages', Result);
end;

function TtgChatMember.Status: string;
begin
  FJSON.TryGetValue<string>('status', Result);
end;

function TtgChatMember.UntilDate: TDateTime;
begin
  Result := ReadToDateTime('until_date');
end;

function TtgChatMember.User: ItgUser;
begin
  Result := ReadToClass<TtgUser>('user');
end;

{ TtgChat }

function TtgChat.AllMembersAreAdministrators: Boolean;
begin
  FJSON.TryGetValue<Boolean>('all_members_are_administrators', Result);
end;

function TtgChat.CanSetStickerSet: Boolean;
begin
  FJSON.TryGetValue<Boolean>('can_set_sticker_set', Result);
end;

function TtgChat.Description: string;
begin
  FJSON.TryGetValue<string>('description', Result);
end;

function TtgChat.FirstName: string;
begin
  FJSON.TryGetValue<string>('first_name', Result);
end;

function TtgChat.ID: Int64;
begin
  FJSON.TryGetValue<Int64>('id', Result);
end;

function TtgChat.InviteLink: string;
begin
  FJSON.TryGetValue<string>('invite_link', Result);
end;

function TtgChat.LastName: string;
begin
  FJSON.TryGetValue<string>('last_name', Result);
end;

function TtgChat.Photo: ItgChatPhoto;
begin
  Result := ReadToClass<TtgChatPhoto>('photo');
end;

function TtgChat.PinnedMessage: ITgMessage;
begin
  Result := ReadToClass<TTgMessage>('pinned_message');
end;

function TtgChat.StickerSetName: string;
begin
  FJSON.TryGetValue<string>('sticker_set_name', Result);
end;

function TtgChat.Title: string;
begin
  FJSON.TryGetValue<string>('title', Result);
end;

function TtgChat.TypeChat: TtgChatType;
begin
  UnSupported;
end;

function TtgChat.Username: string;
begin
  FJSON.TryGetValue<string>('username', Result);
end;

{ TtgSuccessfulPayment }

function TtgSuccessfulPayment.Currency: string;
begin
  FJSON.TryGetValue<string>('currency', Result);
end;

function TtgSuccessfulPayment.InvoicePayload: string;
begin
  FJSON.TryGetValue<string>('invoice_payload', Result);
end;

function TtgSuccessfulPayment.OrderInfo: ItgOrderInfo;
begin
  Result := ReadToClass<TtgOrderInfo>('order_info');
end;

function TtgSuccessfulPayment.ProviderPaymentChargeId: string;
begin
  FJSON.TryGetValue<string>('provider_payment_charge_id', Result);
end;

function TtgSuccessfulPayment.ShippingOptionId: string;
begin
  FJSON.TryGetValue<string>('shipping_option_id', Result);
end;

function TtgSuccessfulPayment.TelegramPaymentChargeId: string;
begin
  FJSON.TryGetValue<string>('telegram_payment_charge_id', Result);
end;

function TtgSuccessfulPayment.TotalAmount: Int64;
begin
  FJSON.TryGetValue<Int64>('total_amount', Result);
end;

{ TtgWebhookInfo }

function TtgWebhookInfo.AllowedUpdates: TArray<string>;
begin
  UnSupported;
end;

function TtgWebhookInfo.HasCustomCertificate: Boolean;
begin
  FJSON.TryGetValue<Boolean>('has_custom_certificate', Result);
end;

function TtgWebhookInfo.LastErrorDate: TDateTime;
begin
  Result := ReadToDateTime('last_error_date');
end;

function TtgWebhookInfo.LastErrorMessage: string;
begin
  FJSON.TryGetValue<string>('last_error_message', Result);
end;

function TtgWebhookInfo.MaxConnections: Int64;
begin
  FJSON.TryGetValue<Int64>('max_connections', Result);
end;

function TtgWebhookInfo.PendingUpdateCount: Int64;
begin
  FJSON.TryGetValue<Int64>('pending_update_count', Result);
end;

function TtgWebhookInfo.Url: string;
begin
  FJSON.TryGetValue<string>('url', Result);
end;

{ TtgMessageEntity }

function TtgMessageEntity.Length: Int64;
begin
  FJSON.TryGetValue<Int64>('length', Result);
end;

function TtgMessageEntity.Offset: Int64;
begin
  FJSON.TryGetValue<Int64>('offset', Result);
end;

function TtgMessageEntity.TypeMessage: TtgMessageEntityType;
begin
  UnSupported;
end;

function TtgMessageEntity.Url: string;
begin
  FJSON.TryGetValue<string>('url', Result);
end;

function TtgMessageEntity.User: ItgUser;
begin
  Result := ReadToClass<TtgUser>('user');
end;

{ TtgAudio }

function TtgAudio.Duration: Int64;
begin
  FJSON.TryGetValue<Int64>('duration', Result);
end;

function TtgAudio.MimeType: string;
begin
  FJSON.TryGetValue<string>('mime_type', Result);
end;

function TtgAudio.Performer: string;
begin
  FJSON.TryGetValue<string>('performer', Result);
end;

function TtgAudio.Title: string;
begin
  FJSON.TryGetValue<string>('title', Result);
end;

{ TtgPhotoSize }

function TtgPhotoSize.Height: Int64;
begin
  FJSON.TryGetValue<Int64>('height', Result);
end;

function TtgPhotoSize.Width: Int64;
begin
  FJSON.TryGetValue<Int64>('width', Result);
end;

{ TtgMaskPosition }

function TtgMaskPosition.Point: TtgMaskPositionPoint;
begin
  UnSupported;
end;

function TtgMaskPosition.Scale: Single;
begin
  FJSON.TryGetValue<Single>('scale', Result);
end;

function TtgMaskPosition.XShift: Single;
begin
  FJSON.TryGetValue<Single>('x_shift', Result);
end;

function TtgMaskPosition.YShift: Single;
begin
  FJSON.TryGetValue<Single>('y_shift', Result);
end;

{ TtgSticker }

function TtgSticker.Emoji: string;
begin
  FJSON.TryGetValue<string>('emoji', Result);
end;

function TtgSticker.Height: Int64;
begin
  FJSON.TryGetValue<Int64>('height', Result);
end;

function TtgSticker.MaskPosition: ItgMaskPosition;
begin
  Result := ReadToClass<TtgMaskPosition>('mask_position');
end;

function TtgSticker.SetName: string;
begin
  FJSON.TryGetValue<string>('set_name', Result);
end;

function TtgSticker.Thumb: ItgPhotoSize;
begin
  Result := ReadToClass<TtgPhotoSize>('thumb');
end;

function TtgSticker.Width: Int64;
begin
  FJSON.TryGetValue<Int64>('width', Result);
end;

{ TtgGame }

function TtgGame.Animation: ItgAnimation;
begin
  Result := ReadToClass<TtgAnimation>('animation');
end;

function TtgGame.Description: string;
begin
  FJSON.TryGetValue<string>('text', Result);
end;

function TtgGame.Photo: TArray<ItgPhotoSize>;
begin
  UnSupported;
end;

function TtgGame.Text: string;
begin
  FJSON.TryGetValue<string>('text', Result);
end;

function TtgGame.TextEntities: TArray<ItgMessageEntity>;
begin

end;

function TtgGame.Title: string;
begin
  FJSON.TryGetValue<string>('title', Result);
end;

{ TtgInvoice }

function TtgInvoice.Currency: string;
begin
  FJSON.TryGetValue<string>('currency', Result);
end;

function TtgInvoice.Description: string;
begin
  FJSON.TryGetValue<string>('description', Result);
end;

function TtgInvoice.StartParameter: string;
begin
  FJSON.TryGetValue<string>('start_parameter', Result);
end;

function TtgInvoice.Title: string;
begin
  FJSON.TryGetValue<string>('title', Result);
end;

function TtgInvoice.TotalAmount: Int64;
begin
  FJSON.TryGetValue<Int64>('total_amount', Result);
end;

{ TtgVideo }

function TtgVideo.Duration: Int64;
begin
  FJSON.TryGetValue<Int64>('duration', Result);
end;

function TtgVideo.Height: Int64;
begin
  FJSON.TryGetValue<Int64>('height', Result);
end;

function TtgVideo.MimeType: string;
begin
  FJSON.TryGetValue<string>('mime_type', Result);
end;

function TtgVideo.Thumb: ItgPhotoSize;
begin
  Result := ReadToClass<TtgPhotoSize>('thumb');
end;

function TtgVideo.Width: Int64;
begin
  FJSON.TryGetValue<Int64>('width', Result);
end;

{ TtgContact }

function TtgContact.FirstName: string;
begin
  FJSON.TryGetValue<string>('first_name', Result);
end;

function TtgContact.LastName: string;
begin
  FJSON.TryGetValue<string>('last_name', Result);
end;

function TtgContact.PhoneNumber: string;
begin
  FJSON.TryGetValue<string>('phone_number', Result);
end;

function TtgContact.UserId: Int64;
begin
  FJSON.TryGetValue<Int64>('user_id', Result);
end;

{ TtgVenue }

function TtgVenue.Address: string;
begin
  FJSON.TryGetValue<string>('address', Result);
end;

function TtgVenue.FoursquareId: string;
begin
  FJSON.TryGetValue<string>('foursquare_id', Result);
end;

function TtgVenue.Location: ItgLocation;
begin
  Result := ReadToClass<TtgLocation>('location');
end;

function TtgVenue.Title: string;
begin
  FJSON.TryGetValue<string>('title', Result);
end;

{ TtgVideoNote }

function TtgVideoNote.Duration: Int64;
begin
  FJSON.TryGetValue<Int64>('duration', Result);
end;

function TtgVideoNote.FileId: string;
begin
  FJSON.TryGetValue<string>('file_id', Result);
end;

function TtgVideoNote.FileSize: Int64;
begin
  FJSON.TryGetValue<Int64>('file_size', Result);
end;

function TtgVideoNote.Length: Int64;
begin
  FJSON.TryGetValue<Int64>('length', Result);
end;

function TtgVideoNote.Thumb: ItgPhotoSize;
begin
  Result := ReadToClass<TtgPhotoSize>('thumb');
end;

{ TtgVoice }

function TtgVoice.Duration: Int64;
begin
  FJSON.TryGetValue<Int64>('duration', Result);
end;

function TtgVoice.MimeType: string;
begin
  FJSON.TryGetValue<string>('mime_type', Result);
end;

{ TtgOrderInfo }

function TtgOrderInfo.Email: string;
begin
  FJSON.TryGetValue<string>('email', Result);
end;

function TtgOrderInfo.Name: string;
begin
  FJSON.TryGetValue<string>('name', Result);
end;

function TtgOrderInfo.PhoneNumber: string;
begin
  FJSON.TryGetValue<string>('phone_number', Result);
end;

function TtgOrderInfo.ShippingAddress: ItgShippingAddress;
begin
  Result := ReadToClass<TtgShippingAddress>('shipping_address');
end;

{ TtgShippingAddress }

function TtgShippingAddress.City: string;
begin
  FJSON.TryGetValue<string>('city', Result);
end;

function TtgShippingAddress.CountryCode: string;
begin
  FJSON.TryGetValue<string>('country_code', Result);
end;

function TtgShippingAddress.PostCode: string;
begin
  FJSON.TryGetValue<string>('post_code', Result);
end;

function TtgShippingAddress.State: string;
begin
  FJSON.TryGetValue<string>('state', Result);
end;

function TtgShippingAddress.StreetLine1: string;
begin
  FJSON.TryGetValue<string>('street_line1', Result);
end;

function TtgShippingAddress.StreetLine2: string;
begin
  Result := ReadToSimpleType<string>('street_line2');
end;

end.
