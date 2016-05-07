unit TelegAPi.Types;

interface

{$DEFINE LANG_EN}

uses
  XSuperObject,
  System.Classes;

Type
  /// <summary> This object represents a Telegram user or bot.</summary>
  TTelegaUser = Class
  private
    FID: Integer;
    FFirstName: String;
    FLastName: String;
    FUsername: String;
  published
    /// <summary> Unique identifier for this user or bot </summary>
    [ALIAS('id')]
    property ID: Integer read FID write FID;
    /// <summary>User‘s or bot’s first name</summary>
    [ALIAS('first_name')]
    property FirstName: String read FFirstName write FFirstName;
    /// <summary>Optional. User‘s or bot’s last name</summary>
    [ALIAS('last_name')]
    property LastName: String read FLastName write FLastName;
    /// <summary>Optional. User‘s or bot’s username</summary>
    [ALIAS('username')]
    property Username: String read FUsername write FUsername;
  End;

  /// <summary>This object represents a chat.</summary>
  TTelegaChat = Class
  private
    FID: Int64;
    Ftype: String;
    Ftitle: String;
    FUsername: String;
    Ffirst_name: String;
    Flast_name: String;
  published
    /// <summary>Unique identifier for this chat, not exceeding 1e13 by absolute value</summary>
    [ALIAS('id')]
    property ID: Int64 read FID write FID;
    /// <summary> Type of chat, can be either “private”, “group”, “supergroup” or “channel”</summary>
    [ALIAS('type')]
    property &type: String read Ftype write Ftype;
    /// <summary>Optional. Title, for channels and group chats </summary>
    [ALIAS('title')]
    property title: String read Ftitle write Ftitle;
    /// <summary>Optional. Username, for private chats and channels if available </summary>
    [ALIAS('username')]
    property Username: String read FUsername write FUsername;
    /// <summary>Optional. First name of the other party in a private chat </summary>
    [ALIAS('first_name')]
    property first_name: String read Ffirst_name write Ffirst_name;
    /// <summary>Optional. Last name of the other party in a private chat </summary>
    [ALIAS('last_name')]
    property last_name: String read Flast_name write Flast_name;
  End;

  /// <summary>This object represents one special entity in a text message. For example, hashtags, usernames, URLs, etc.</summary>
  TTelegaMessageEntity = Class
  private
    Ftype: String;
    Foffset: Integer;
    Flength: Integer;
    Furl: String;
  published
    /// <summary>Type of the entity. One of mention (@username), hashtag, bot_command, url, email, bold (bold text), italic (italic text), code (monowidth string), pre (monowidth block), text_link (for clickable text URLs)</summary>
    [ALIAS('type')]
    property &type: String read Ftype write Ftype;
    /// <summary>Offset in UTF-16 code units to the start of the entity</summary>
    [ALIAS('offset')]
    property offset: Integer read Foffset write Foffset;
    /// <summary>Length of the entity in UTF-16 code units</summary>
    [ALIAS('length')]
    property length: Integer read Flength write Flength;
    /// <summary>Optional. For “text_link” only, url that will be opened after user taps on the text</summary>
    [ALIAS('url')]
    property url: String read Furl write Furl;
  End;

  /// <summary>This object represents a file ready to be downloaded. The file can be downloaded via the link https://api.telegram.org/file/bot<token>/<file_path>. It is guaranteed that the link will be valid for at least 1 hour. When the link expires, a new one can be requested by calling getFile. </summary>
  /// <remarks>Maximum file size to download is 20 MB </remarks>
  TTelegaFile = Class
  private
    FFileId: String;
    FFileSize: Integer;
    FFilePath: String;
    FFileStream: TStream;
  published
    /// <summary>Unique identifier for this file</summary>
    [ALIAS('file_id')]
    property FileId: String read FFileId write FFileId;
    /// <summary>Optional. File size, if known</summary>
    [ALIAS('file_size')]
    property FileSize: Integer read FFileSize write FFileSize;
    /// <summary>File path. Use https://api.telegram.org/file/bot{token}/{file_path} to get the file.</summary>
    [ALIAS('file_path')]
    property FilePath: String read FFilePath write FFilePath;
    property FileStream: TStream read FFileStream write FFileStream;
  End;

  /// <summary>This object represents an audio file to be treated as music by the Telegram clients.</summary>
  TTelegaAudio = Class(TTelegaFile)
  private
    FDuration: Integer;
    FPerformer: String;
    Ftitle: String;
    FMimeType: String;
  published
    /// <summary>Duration of the audio in seconds as defined by sender</summary>
    [ALIAS('duration')]
    property Duration: Integer read FDuration write FDuration;
    /// <summary>Performer of the audio as defined by sender or by audio tags</summary>
    [ALIAS('performer')]
    property Performer: String read FPerformer write FPerformer;
    /// <summary>Title of the audio as defined by sender or by audio tags</summary>
    [ALIAS('title')]
    property title: String read Ftitle write Ftitle;
    /// <summary>Optional. MIME type of the file as defined by sender</summary>
    [ALIAS('mime_type')]
    property MimeType: String read FMimeType write FMimeType;
  End;

  /// <summary>This object represents one size of a photo or a file/sticker thumbnail.</summary>
  /// <remarks>A missing thumbnail for a file (or sticker) is presented as an empty object.</remarks>
  TTelegaPhotoSize = Class(TTelegaFile)
  private
    FWidth: Integer;
    FHeight: Integer;
  published
    /// <summary>Photo width</summary>
    [ALIAS('width')]
    property Width: Integer read FWidth write FWidth;
    /// <summary>Photo height</summary>
    [ALIAS('Height')]
    property Height: Integer read FHeight write FHeight;
  End;

  /// <summary>This object represents a general file (as opposed to photos, voice messages and audio files).</summary>
  TTelegaDocument = Class(TTelegaFile)
  private
    FThumb: TTelegaPhotoSize;
    FFileName: String;
    FMimeType: String;
  published
    /// <summary>Document thumbnail as defined by sender</summary>
    [ALIAS('thumb')]
    property Thumb: TTelegaPhotoSize read FThumb write FThumb;
    /// <summary>Optional. Original filename as defined by sender</summary>
    [ALIAS('file_name')]
    property FileName: String read FFileName write FFileName;
    /// <summary>Optional. MIME type of the file as defined by sender</summary>
    [ALIAS('mime_type')]
    property MimeType: String read FMimeType write FMimeType;
  End;

  /// <summary>This object represents a sticker.</summary>
  TTelegaSticker = Class(TTelegaFile)
  private
    FWidth: Integer;
    FHeight: Integer;
    FThumb: TTelegaPhotoSize;
  published
    /// <summary>Sticker width</summary>
    [ALIAS('width')]
    property Width: Integer read FWidth write FWidth;
    /// <summary>Sticker height</summary>
    [ALIAS('width')]
    property Height: Integer read FHeight write FHeight;
    /// <summary>Sticker thumbnail in .webp or .jpg format</summary>
    [ALIAS('thumb')]
    property Thumb: TTelegaPhotoSize read FThumb write FThumb;
  End;

  /// <summary>This object represents a video file.</summary>
  TTelegaVideo = Class(TTelegaFile)
  private
    FWidth: Integer;
    FHeight: Integer;
    FDuration: Integer;
    FThumb: TTelegaPhotoSize;
    FMimeType: String;
  published
    /// <summary>Video width as defined by sender</summary>
    [ALIAS('width')]
    property Width: Integer read FWidth write FWidth;
    /// <summary>Video height as defined by sender</summary>
    [ALIAS('height')]
    property Height: Integer read FHeight write FHeight;
    /// <summary>Duration of the video in seconds as defined by sender</summary>
    [ALIAS('duration')]
    property Duration: Integer read FDuration write FDuration;
    /// <summary>Video thumbnail</summary>
    [ALIAS('thumb')]
    property Thumb: TTelegaPhotoSize read FThumb write FThumb;
    /// <summary>Optional. Mime type of a file as defined by sender</summary>
    [ALIAS('mime_type')]
    property MimeType: String read FMimeType write FMimeType;
  End;

  /// <summary>This object represents a voice note.</summary>
  TTelegaVoice = Class(TTelegaFile)
  private
    FDuration: Integer;
    FMimeType: String;
  published
    /// <summary>Duration of the audio in seconds as defined by sender</summary>
    [ALIAS('duration')]
    property Duration: Integer read FDuration write FDuration;
    /// <summary>Optional. MIME type of the file as defined by sender</summary>
    [ALIAS('mime_type')]
    property MimeType: String read FMimeType write FMimeType;
  End;

  /// <summary>This object represents a phone contact.</summary>
  TTelegaContact = Class
  private
    FPhoneNumber: String;
    FFirstName: String;
    FLastName: String;
    FUserId: Integer;
  published
    /// <summary>Contact's phone number</summary>
    [ALIAS('phone_number')]
    property PhoneNumber: String read FPhoneNumber write FPhoneNumber;
    /// <summary>Contact's first name </summary>
    [ALIAS('first_name')]
    property FirstName: String read FFirstName write FFirstName;
    /// <summary>Optional. Contact's last name</summary>
    [ALIAS('last_name')]
    property LastName: String read FLastName write FLastName;
    /// <summary>Optional. Contact's user identifier in Telegram </summary>
    [ALIAS('user_id')]
    property UserId: Integer read FUserId write FUserId;
  End;

  /// <summary>This object represents a point on the map.</summary>
  TTelegaLocation = Class
  private
    FLongitude: Single;
    FLatitude: Single;
  published
    /// <summary>Longitude as defined by sender</summary>
    [ALIAS('longitude')]
    property Longitude: Single read FLongitude write FLongitude;
    /// <summary>Latitude as defined by sender</summary>
    [ALIAS('latitude')]
    property Latitude: Single read FLatitude write FLatitude;
  End;

  /// <summary>This object represents a venue.</summary>
  TTelegaVenue = Class
  private
    FLocation: TTelegaLocation;
    Ftitle: String;
    FAddress: String;
    FFoursquareId: String;
  published
    /// <summary>Venue location</summary>
    [ALIAS('location')]
    property Location: TTelegaLocation read FLocation write FLocation;
    /// <summary>Title of the result</summary>
    [ALIAS('title')]
    property title: String read Ftitle write Ftitle;
    /// <summary>Address of the venue </summary>
    [ALIAS('address')]
    property Address: String read FAddress write FAddress;
    /// <summary> Optional. Foursquare identifier of the venue</summary>
    [ALIAS('foursquare_id')]
    property FoursquareId: String read FFoursquareId write FFoursquareId;
  End;
{$SCOPEDENUMS ON}

  /// <summary>The type of a Message</summary>
  TTelegaMessageType = (UnknownMessage = 0, TextMessage, PhotoMessage, AudioMessage, VideoMessage,
    VoiceMessage, DocumentMessage, StickerMessage, LocationMessage, ContactMessage, ServiceMessage,
    VenueMessage);
{$SCOPEDENUMS OFF}

  /// <summary>This object represents a message.</summary>
  TTelegaMessage = Class
  private
    FMessageId: Integer;
    FFrom: TTelegaUser;
    FDate: Integer;
    FChat: TTelegaChat;
    FForwardFrom: TTelegaUser;
    FForwardDate: Integer;
    FReplyToMessage: TTelegaMessage;
    FText: String;
    FAudio: TTelegaAudio;
    FDocument: TTelegaDocument;
    FPhoto: TArray<TTelegaPhotoSize>;
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
    FNewChatPhoto: TArray<TTelegaPhotoSize>;
    FDeleteChatPhoto: Boolean;
    FGroupChatCreated: Boolean;
    FSupergroupChatCreated: Boolean;
    FChannelChatCreated: Boolean;
    FMigrateToChatId: Int64;
    FMigrateFromChatId: Int64;
    FPinnedMessage: TTelegaMessage;
    Fentities: TArray<TTelegaMessageEntity>;
  public
    /// <summary>Unique message identifier</summary>
    [ALIAS('message_id')]
    property MessageId: Integer read FMessageId write FMessageId;
    /// <summary>Sender</summary>
    [ALIAS('from')]
    property From: TTelegaUser read FFrom write FFrom;
    /// <summary>Date the message was sent in Unix time</summary>
    [ALIAS('date')]
    property Date: Integer read FDate write FDate;
    /// <summary>Conversation the message belongs to</summary>
    [ALIAS('chat')]
    property Chat: TTelegaChat read FChat write FChat;
    /// <summary>Optional. For forwarded messages, sender of the original message</summary>
    [ALIAS('forward_from')]
    property ForwardFrom: TTelegaUser read FForwardFrom write FForwardFrom;
    /// <summary>Optional. For forwarded messages, date the original message was sent in Unix time</summary>
    [ALIAS('forward_date')]
    property ForwardDate: Integer read FForwardDate write FForwardDate;
    /// <summary>Optional. For replies, the original message. Note that the Message object in this field will not contain further reply_to_message fields even if it itself is a reply.</summary>
    [ALIAS('reply_to_message')]
    property ReplyToMessage: TTelegaMessage read FReplyToMessage write FReplyToMessage;
    /// <summary>Optional. For text messages, the actual UTF-8 text of the message</summary>
    [ALIAS('text')]
    property Text: String read FText write FText;
    /// <summary>Optional. For text messages, special entities like usernames, URLs, bot commands, etc. that appear in the text</summary>
    [ALIAS('entities')]
    property entities: TArray<TTelegaMessageEntity> read Fentities write Fentities;
    /// <summary>Optional. Message is an audio file, information about the file</summary>
    [ALIAS('audio')]
    property Audio: TTelegaAudio read FAudio write FAudio;
    /// <summary>Optional. Message is a general file, information about the file</summary>
    [ALIAS('document')]
    property Document: TTelegaDocument read FDocument write FDocument;
    /// <summary>Optional. Message is a photo, available sizes of the photo</summary>
    [ALIAS('photo')]
    property Photo: TArray<TTelegaPhotoSize> read FPhoto write FPhoto;
    /// <summary>Optional. Message is a sticker, information about the sticker</summary>
    [ALIAS('sticker')]
    property Sticker: TTelegaSticker read FSticker write FSticker;
    /// <summary>Optional. Message is a video, information about the video</summary>
    [ALIAS('video')]
    property Video: TTelegaVideo read FVideo write FVideo;
    /// <summary>Message is a voice message, information about the file</summary>
    [ALIAS('voice')]
    property Voice: TTelegaVoice read FVoice write FVoice;
    /// <summary>Optional. Caption for the document, photo or video, 0-200 characters</summary>
    [ALIAS('caption')]
    property Caption: String read FCaption write FCaption;
    /// <summary>Optional. Message is a shared contact, information about the contact</summary>
    [ALIAS('contact')]
    property Contact: TTelegaContact read FContact write FContact;
    /// <summary>Optional. Message is a shared location, information about the location</summary>
    [ALIAS('location')]
    property Location: TTelegaLocation read FLocation write FLocation;
    /// <summary>Optional. Message is a venue, information about the venue</summary>
    [ALIAS('venue')]
    property Venue: TTelegaVenue read FVenue write FVenue;
    /// <summary>Optional. A new member was added to the group, information about them (this member may be bot itself)</summary>
    [ALIAS('new_chat_member')]
    property NewChatMember: TTelegaUser read FNewChatMember write FNewChatMember;
    /// <summary>Optional. A member was removed from the group, information about them (this member may be bot itself)</summary>
    [ALIAS('left_chat_member')]
    property LeftChatMember: TTelegaUser read FLeftChatMember write FLeftChatMember;
    /// <summary>Optional. A group title was changed to this value</summary>
    [ALIAS('new_chat_title')]
    property NewChatTitle: String read FNewChatTitle write FNewChatTitle;
    /// <summary>Optional. A group photo was change to this value</summary>
    [ALIAS('new_chat_photo')]
    property NewChatPhoto: TArray<TTelegaPhotoSize> read FNewChatPhoto write FNewChatPhoto;
    /// <summary>Optional. Informs that the group photo was deleted</summary>
    [ALIAS('delete_chat_photo')]
    property DeleteChatPhoto: Boolean read FDeleteChatPhoto write FDeleteChatPhoto;
    /// <summary>Optional. Informs that the group has been created</summary>
    [ALIAS('group_chat_created')]
    property GroupChatCreated: Boolean read FGroupChatCreated write FGroupChatCreated;
    /// <summary>Optional. Service message: the supergroup has been created</summary>
    [ALIAS('supergroup_chat_created')]
    property SupergroupChatCreated: Boolean read FSupergroupChatCreated
      write FSupergroupChatCreated;
    /// <summary> Optional. Service message: the channel has been created </summary>
    [ALIAS('channel_chat_created')]
    property ChannelChatCreated: Boolean read FChannelChatCreated write FChannelChatCreated;
    /// <summary> Optional. The group has been migrated to a supergroup with the specified identifier</summary>
    [ALIAS('migrate_to_chat_id')]
    property MigrateToChatId: Int64 read FMigrateToChatId write FMigrateToChatId;
    /// <summary>Optional. The supergroup has been migrated from a group with the specified identifier</summary>
    [ALIAS('migrate_from_chat_id')]
    property MigrateFromChatId: Int64 read FMigrateFromChatId write FMigrateFromChatId;
    /// <summary>Optional. Specified message was pinned. Note that the Message object in this field will not contain further reply_to_message fields even if it is itself a reply</summary>
    [ALIAS('pinned_message')]
    property PinnedMessage: TTelegaMessage read FPinnedMessage write FPinnedMessage;
  End;
{$SCOPEDENUMS ON}

  /// <summary>Text parsing mode</summary>
  TTelegaParseMode = (Default = 0, Markdown, Html);
{$SCOPEDENUMS OFF}

  /// <summary>This object represent a user's profile pictures.</summary>
  TTelegaUserProfilePhotos = Class
  private
    Ftotal_count: Integer;
    Fphotos: TArray<TArray<TTelegaPhotoSize>>;
  published
    /// <summary>Total number of profile pictures the target user has</summary>
    [ALIAS('total_count')]
    property total_count: Integer read Ftotal_count write Ftotal_count;
    /// <summary>Requested profile pictures (in up to 4 sizes each)</summary>
    [ALIAS('photos')]
    property photos: TArray < TArray < TTelegaPhotoSize >> read Fphotos write Fphotos;
  End;

  /// <summary>This object represents one button of the reply keyboard. For simple text buttons String can be used instead of this object to specify text of the button. Optional fields are mutually exclusive.</summary>
  /// <remarks>request_contact and request_location options will only work in Telegram versions released after 9 April, 2016. Older clients will ignore them.</remarks>
  TTelegaKeyboardButton = Class
  private
    FText: String;
    Frequest_contact: Boolean;
    Frequest_location: Boolean;
  Public
    constructor Create(Text: String; request_contact: Boolean = False;
      request_location: Boolean = False); overload;
  published
    /// <summary>Text of the button. If none of the optional fields are used, it will be sent to the bot as a message when the button is pressed</summary>
    [ALIAS('text')]
    property Text: String read FText write FText;
    /// <summary>Optional. If True, the user's phone number will be sent as a contact when the button is pressed. Available in private chats only</summary>
    [ALIAS('request_contact')]
    property request_contact: Boolean read Frequest_contact write Frequest_contact;
    /// <summary>Optional. If True, the user's current location will be sent when the button is pressed. Available in private chats only</summary>
    [ALIAS('request_location')]
    property request_location: Boolean read Frequest_location write Frequest_location;
  End;

  /// <summary>Upon receiving a message with this object, Telegram clients will hide the current custom keyboard and display the default letter-keyboard. By default, custom keyboards are displayed until a new keyboard is sent by a bot. An exception is made for one-time keyboards that are hidden immediately after the user presses a button (see ReplyKeyboardMarkup).</summary>
  TTelegaReplyKeyboardHide = Class
  private
    Fhide_keyboard: Boolean;
    Fphotos: TArray<TArray<TTelegaPhotoSize>>;
    Fselective: Boolean;
  published
    /// <summary>Requested profile pictures (in up to 4 sizes each)</summary>
    [ALIAS('hide_keyboard')]
    property hide_keyboard: Boolean read Fhide_keyboard write Fhide_keyboard;
    /// <summary>Optional. Use this parameter if you want to hide keyboard for specific users only. Targets: 1) users that are @mentioned in the text of the Message object; 2) if the bot's message is a reply (has reply_to_message_id), sender of the original message.</summary>
    /// <remarks>Example: A user votes in a poll, bot returns confirmation message in reply to the vote and hides keyboard for that user, while still showing the keyboard with poll options to users who haven't voted yet.</remarks>
    [ALIAS('selective')]
    property selective: Boolean read Fselective write Fselective;
  End;

  /// <summary>This object represents a custom keyboard with reply options (see Introduction to bots for details and examples).</summary>
  TTelegaReplyKeyboardMarkup = Class
  private
    Fresize_keyboard: Boolean;
    FKeyBoard: TArray<TArray<TTelegaKeyboardButton>>;
    Fone_time_keyboard: Boolean;
    Fselective: Boolean;
  published
    /// <summary>Array of button rows, each represented by an Array of KeyboardButton objects</summary>
    [ALIAS('keyboard')]
    property KeyBoard: TArray < TArray < TTelegaKeyboardButton >> read FKeyBoard write FKeyBoard;
    /// <summary>Optional. Requests clients to resize the keyboard vertically for optimal fit (e.g., make the keyboard smaller if there are just two rows of buttons). Defaults to false, in which case the custom keyboard is always of the same height as the app's standard keyboard.</summary>
    [ALIAS('resize_keyboard')]
    property resize_keyboard: Boolean read Fresize_keyboard write Fresize_keyboard;
    /// <summary>Optional. Requests clients to hide the keyboard as soon as it's been used. The keyboard will still be available, but clients will automatically display the usual letter-keyboard in the chat – the user can press a special button in the input field to see the custom keyboard again. Defaults to false.</summary>
    [ALIAS('one_time_keyboard')]
    property one_time_keyboard: Boolean read Fone_time_keyboard write Fone_time_keyboard;
    /// <summary>Optional. Use this parameter if you want to show the keyboard to specific users only. Targets: 1) users that are @mentioned in the text of the Message object; 2) if the bot's message is a reply (has reply_to_message_id), sender of the original message.</summary>
    [ALIAS('selective')]
    property selective: Boolean read Fselective write Fselective;
  End;

  /// <summary>This object represents one button of an inline keyboard. You must use exactly one of the optional fields.</summary>
  TTelegaInlineKeyboardButton = Class
  private
    FText: String;
    Furl: String;
    Fcallback_data: String;
    Fswitch_inline_query: String;
  published
    /// <summary>Label text on the button</summary>
    [ALIAS('text')]
    property Text: String read FText write FText;
    /// <summary>Optional. HTTP url to be opened when button is pressed</summary>
    [ALIAS('url')]
    property url: String read Furl write Furl;
    /// <summary>Optional. Data to be sent in a callback query to the bot when button is pressed, 1-64 bytes</summary>
    [ALIAS('callback_data')]
    property callback_data: String read Fcallback_data write Fcallback_data;
    /// <summary>Optional. If set, pressing the button will prompt the user to select one of their chats, open that chat and insert the bot‘s username and the specified inline query in the input field. Can be empty, in which case just the bot’s username will be inserted.</summary>
    /// <remarks>Note: This offers an easy way for users to start using your bot in inline mode when they are currently in a private chat with it. Especially useful when combined with switch_pm… actions – in this case the user will be automatically returned to the chat they switched from, skipping the chat selection screen.</remarks>
    [ALIAS('switch_inline_query')]
    property switch_inline_query: String read Fswitch_inline_query write Fswitch_inline_query;
  End;

  /// <summary>This object represents an inline keyboard that appears right next to the message it belongs to.</summary>
  /// <remarks>Warning: Inline keyboards are currently being tested and are only available in one-on-one chats (i.e., user-bot or user-user in the case of inline bots).</remarks>
  TTelegaInlineKeyboardMarkup = Class
  private
    Finline_keyboard: TArray<TArray<TTelegaInlineKeyboardButton>>;
  published
    /// <summary>Array of button rows, each represented by an Array of InlineKeyboardButton objects</summary>
    [ALIAS('inline_keyboard')]
    property inline_keyboard: TArray < TArray < TTelegaInlineKeyboardButton >> read Finline_keyboard
      write Finline_keyboard;

  End;

  TTelegaApiResponse<T> = Class
  private
    FOk: Boolean;
    FResultObject: T;
    FMessage: String;
    FCode: Integer;
  published
    /// <summary> Gets a value indicating whether the request was successful.</summary>
    [ALIAS('ok')]
    property Ok: Boolean read FOk write FOk;
    /// <summary>Gets the result object.</summary>
    /// <value>The result object.</value>
    [ALIAS('result')]
    property ResultObject: T read FResultObject write FResultObject;
    /// <summary>Gets the error message.</summary>
    /// <value>The error message.</value>
    [ALIAS('description')]
    property Message: String read FMessage write FMessage;
    /// <summary>Gets the error code.</summary>
    /// <value>The error code</value>
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

  /// <summary>This object represents an incoming inline query. When the user sends an empty query, your bot could return some default or trending results.</summary>
  TTelegaInlineQuery = Class
  private
    FID: String;
    FFrom: TTelegaUser;
    FQuery: String;
    Foffset: String;
  published
    /// <summary>Unique identifier for this query</summary>
    [ALIAS('id')]
    property ID: String read FID write FID;
    /// <summary>Sender</summary>
    [ALIAS('from')]
    property From: TTelegaUser read FFrom write FFrom;
    /// <summary>Text of the query</summary>
    [ALIAS('query')]
    property Query: String read FQuery write FQuery;
    /// <summary>Offset of the results to be returned, can be controlled by the bot</summary>
    [ALIAS('offset')]
    property offset: String read Foffset write Foffset;
  End;

  /// <summary>This object represents a result of an inline query that was chosen by the user and sent to their chat partner.</summary>
  TTelegaChosenInlineResult = Class
  private
    FResultId: String;
    FFrom: TTelegaUser;
    FQuery: String;
  published
    /// <summary>The unique identifier for the result that was chosen.</summary>
    [ALIAS('result_id')]
    property ResultId: String read FResultId write FResultId;
    /// <summary>The user that chose the result.</summary>
    [ALIAS('from')]
    property From: TTelegaUser read FFrom write FFrom;
    /// <summary>The query that was used to obtain the result.</summary>
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
    /// <summary>Unique identifier for this query</summary>
    [ALIAS('id')]
    property ID: String read FID write FID;
    /// <summary>Sender</summary>
    [ALIAS('from')]
    property From: TTelegaUser read FFrom write FFrom;
    /// <summary>Optional. Message with the callback button that originated the query. Note that message content and message date will not be available if the message is too old</summary>
    [ALIAS('message')]
    property Message: TTelegaMessage read FMessage write FMessage;
    /// <summary>Optional. Identifier of the message sent via the bot in inline mode, that originated the query</summary>
    [ALIAS('inline_message_id')]
    property InlineMessageId: String read FInlineMessageId write FInlineMessageId;
    /// <summary>Data associated with the callback button. Be aware that a bad client can send arbitrary data in this field</summary>
    [ALIAS('data')]
    property Data: String read FData write FData;
  End;

  /// <summary>Upon receiving a message with this object, Telegram clients will display a reply interface to the user (act as if the user has selected the bot‘s message and tapped ’Reply'). This can be extremely useful if you want to create user-friendly step-by-step interfaces without having to sacrifice privacy mode. </summary>
  TTelegaForceReply = Class
  private
    Fforce_reply: Boolean;
    Fselective: Boolean;
  published
    /// <summary>Shows reply interface to the user, as if they manually selected the bot‘s message and tapped ’Reply'</summary>
    [ALIAS('force_reply')]
    property force_reply: Boolean read Fforce_reply write Fforce_reply;
    /// <summary>Optional. Use this parameter if you want to force reply from specific users only. Targets: 1) users that are @mentioned in the text of the Message object; 2) if the bot's message is a reply (has reply_to_message_id), sender of the original message.</summary>
    [ALIAS('selective')]
    property selective: Boolean read Fselective write Fselective;
  End;

{$SCOPEDENUMS ON}

  /// <summary>The type of an Update</summary>
  TTelegaUpdateType = (UnkownUpdate = 0, MessageUpdate, InlineQueryUpdate, ChosenInlineResultUpdate,
    CallbackQueryUpdate);
{$SCOPEDENUMS OFF}

  /// <summary>=This object represents an incoming update.</summary>
  /// <remarks>Only one of the optional parameters can be present in any given update.</remarks>
  TTelegaUpdate = Class
  private
    FID: Integer;
    FMessage: TTelegaMessage;
    FInlineQuery: TTelegaInlineQuery;
    FChosenInlineResult: TTelegaChosenInlineResult;
    FCallbackQuery: TTelegaCallbackQuery;
    function Get: TTelegaUpdateType;
  public
  published
    /// <summary>The update‘s unique identifier. Update identifiers start from a certain positive number and increase sequentially. This ID becomes especially handy if you’re using Webhooks, since it allows you to ignore repeated updates or to restore the correct update sequence, should they get out of order. </summary>
    [ALIAS('update_id')]
    property ID: Integer read FID write FID;
    /// <summary>Optional. New incoming message of any kind — text, photo, sticker, etc.</summary>
    [ALIAS('message')]
    property Message: TTelegaMessage read FMessage write FMessage;
    /// <summary>Optional. New incoming inline query</summary>
    [ALIAS('inline_query')]
    property InlineQuery: TTelegaInlineQuery read FInlineQuery write FInlineQuery;
    /// <summary>Optional. The result of a inline query that was chosen by a user and sent to their chat partner</summary>
    [ALIAS('chosen_inline_result')]
    property ChosenInlineResult: TTelegaChosenInlineResult read FChosenInlineResult
      write FChosenInlineResult;
    /// <summary>Optional. New incoming callback query</summary>
    [ALIAS('callback_query')]
    property CallbackQuery: TTelegaCallbackQuery read FCallbackQuery write FCallbackQuery;
    property &type: TTelegaUpdateType read Get;
  End;

  /// <summary>This object represents one result of an inline query. Telegram clients currently support results of the following 19 types   /// </summary>
  [ALIAS('InlineQueryResult')]
  TTelegaInlineQueryResult = Class
  private
    Ftype: String;
    FID: String;
  published
    /// <summary>Type of the result</summary>
    [ALIAS('type')]
    property &type: String read Ftype write Ftype;
    /// <summary>Unique identifier for this result, 1-64 bytes</summary>
    [ALIAS('id')]
    property ID: String read FID write FID;
  End;

  /// <summary>Represents a link to an article or web page.</summary>
  [ALIAS('InlineQueryResultArticle')]
  TTelegaInlineQueryResultArticle = class(TTelegaInlineQueryResult)

  end;

implementation

uses
  System.SysUtils;

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

{ TTelegaKeyboardButton }

constructor TTelegaKeyboardButton.Create(Text: String; request_contact, request_location: Boolean);
begin
  FText := Text;
  Frequest_contact := request_contact;
  Frequest_location := request_location;
end;

end.
