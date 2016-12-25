unit TelegAPi.Classes;

interface

uses
  XSuperObject,
  System.Classes;

Type
{$SCOPEDENUMS ON}
  /// <summary>The type of a Message</summary>
  TtgMessageType = (UnknownMessage = 0, TextMessage, PhotoMessage, AudioMessage, VideoMessage,
    VoiceMessage, DocumentMessage, StickerMessage, LocationMessage, ContactMessage, ServiceMessage,
    VenueMessage);
  /// <summary>
  ///   Text parsing mode
  /// </summary>
  /// <example>
  ///   <para>
  ///     Markdown style
  ///   </para>
  ///   <para>
  ///     *bold text* <br />_italic text_ <br />
  ///     [text](http://www.example.com/) <br />`inline fixed-width code` <br />
  ///     ```text <br />pre-formatted fixed-width code block <br />```
  ///   </para>
  ///   <para>
  ///     Html:
  ///   </para>
  ///   <para>
  ///     &lt;b&gt;bold&lt;/b&gt;, &lt;strong&gt;bold&lt;/strong&gt; <br />
  ///     &lt;i&gt;italic&lt;/i&gt;, &lt;em&gt;italic&lt;/em&gt; <br />&lt;a
  ///     href="http://www.example.com/"&gt;inline URL&lt;/a&gt; <br />
  ///     &lt;code&gt;inline fixed-width code&lt;/code&gt; <br />
  ///     &lt;pre&gt;pre-formatted fixed-width code block&lt;/pre&gt; <br /><br />
  ///   </para>
  /// </example>
  TtgParseMode = (Default = 0,
    /// <summary>
    ///   To use this mode, pass Markdown in the parse_mode field when using
    ///   sendMessage
    /// </summary>
    Markdown,
    /// <summary>
    ///   To use this mode, pass HTML in the parse_mode field when using
    ///   sendMessage
    /// </summary>
    Html);
  /// <summary>The type of an Update</summary>
  TtgUpdateType = (UnkownUpdate = 0, MessageUpdate, InlineQueryUpdate, ChosenInlineResultUpdate,
    CallbackQueryUpdate);
  TAllowedUpdate = (message, edited_message, channel_post, edited_channel_post, inline_query, chosen_inline_result, callback_query);
  TAllowedUpdates = set of TAllowedUpdate;
{$SCOPEDENUMS OFF}
{$M+}

  [Alias('User')]
  /// <summary> This object represents a Telegram user or bot.</summary>
  TtgUser = Class
  private
    FID: Integer;
    FFirstName: String;
    FLastName: String;
    FUsername: String;
  published
    /// <summary> Unique identifier for this user or bot </summary>
    [Alias('id')]
    property ID: Integer read FID write FID;
    /// <summary>User‘s or bot’s first name</summary>
    [Alias('first_name')]
    property FirstName: String read FFirstName write FFirstName;
    /// <summary>Optional. User‘s or bot’s last name</summary>
    [Alias('last_name')]
    property LastName: String read FLastName write FLastName;
    /// <summary>Optional. User‘s or bot’s username</summary>
    [Alias('username')]
    property Username: String read FUsername write FUsername;
  End;

  /// <summary>This object contains information about one member of the chat.</summary>
  [Alias('ChatMember')]
  TtgChatMember = Class
  private
    Fstatus: String;
    Fuser: TtgUser;
  public
    destructor Destroy; override;
  published
    /// <summary>Information about the user</summary>
    [Alias('user')]
    property user: TtgUser read Fuser write Fuser;
    /// <summary>The member's status in the chat. Can be “creator”, “administrator”, “member”, “left” or “kicked”</summary>
    [Alias('status')]
    property status: String read Fstatus write Fstatus;
  End;

  /// <summary>This object represents a chat.</summary>
  [Alias('Chat')]
  TtgChat = Class
  private
    FID: Int64;
    Ftype: String;
    Ftitle: String;
    FUsername: String;
    Ffirst_name: String;
    Flast_name: String;
    Fall_members_are_administrators: Boolean;
  published
    /// <summary>Unique identifier for this chat, not exceeding 1e13 by absolute value</summary>
    [Alias('id')]
    property ID: Int64 read FID write FID;
    /// <summary> Type of chat, can be either “private”, “group”, “supergroup” or “channel”</summary>
    [Alias('type')]
    property &type: String read Ftype write Ftype;
    /// <summary>Optional. Title, for channels and group chats </summary>
    [Alias('title')]
    property title: String read Ftitle write Ftitle;
    /// <summary>Optional. Username, for private chats and channels if available </summary>
    [Alias('username')]
    property Username: String read FUsername write FUsername;
    /// <summary>Optional. First name of the other party in a private chat </summary>
    [Alias('first_name')]
    property first_name: String read Ffirst_name write Ffirst_name;
    /// <summary>Optional. Last name of the other party in a private chat </summary>
    [Alias('last_name')]
    property last_name: String read Flast_name write Flast_name;
    /// <summary>Optional. True if a group has ‘All Members Are Admins’ enabled.</summary>
    [Alias('all_members_are_administrators')]
    property all_members_are_administrators: Boolean read Fall_members_are_administrators write Fall_members_are_administrators;
  End;

  /// <summary>This object represents one special entity in a text message. For example, hashtags, usernames, URLs, etc.</summary>
  [Alias('MessageEntity')]
  TtgMessageEntity = Class
  private
    Ftype: String;
    Foffset: Integer;
    Flength: Integer;
    Furl: String;
    Fuser: TtgUser;
  public
    destructor Destroy; override;
  published
    /// <summary>Type of the entity. One of mention (@username), hashtag, bot_command, url, email, bold (bold text), italic (italic text), code (monowidth string), pre (monowidth block), text_link (for clickable text URLs), text_mention (for users without usernames)</summary>
    [Alias('type')]
    property &type: String read Ftype write Ftype;
    /// <summary>Offset in UTF-16 code units to the start of the entity</summary>
    [Alias('offset')]
    property offset: Integer read Foffset write Foffset;
    /// <summary>Length of the entity in UTF-16 code units</summary>
    [Alias('length')]
    property length: Integer read Flength write Flength;
    /// <summary>Optional. For “text_link” only, url that will be opened after user taps on the text</summary>
    [Alias('url')]
    property url: String read Furl write Furl;
    /// <summary>Optional. For “text_mention” only, the mentioned user</summary>
    [Alias('user')]
    property user: TtgUser read Fuser write Fuser;
  End;

  /// <summary>This object represents a file ready to be downloaded. The file can be downloaded via the link https://api.telegram.org/file/bot<token>/<file_path>. It is guaranteed that the link will be valid for at least 1 hour. When the link expires, a new one can be requested by calling getFile. </summary>
  /// <remarks>Maximum file size to download is 20 MB </remarks>
  [Alias('File')]
  TtgFile = Class
  private
    FFileId: String;
    FFileSize: Integer;
    FFilePath: String;
  public
  published
    /// <summary>Unique identifier for this file</summary>
    [Alias('file_id')]
    property FileId: String read FFileId write FFileId;
    /// <summary>Optional. File size, if known</summary>
    [Alias('file_size')]
    property FileSize: Integer read FFileSize write FFileSize;
    /// <summary>File path. Use https://api.telegram.org/file/bot{token}/{file_path} to get the file.</summary>
    [Alias('file_path')]
    property FilePath: String read FFilePath write FFilePath;
  End;

  /// <summary>This object represents an audio file to be treated as music by the Telegram clients.</summary>
  [Alias('Audio')]
  TtgAudio = Class(TtgFile)
  private
    FDuration: Integer;
    FPerformer: String;
    Ftitle: String;
    FMimeType: String;
  published
    /// <summary>Duration of the audio in seconds as defined by sender</summary>
    [Alias('duration')]
    property Duration: Integer read FDuration write FDuration;
    /// <summary>Performer of the audio as defined by sender or by audio tags</summary>
    [Alias('performer')]
    property Performer: String read FPerformer write FPerformer;
    /// <summary>Title of the audio as defined by sender or by audio tags</summary>
    [Alias('title')]
    property title: String read Ftitle write Ftitle;
    /// <summary>Optional. MIME type of the file as defined by sender</summary>
    [Alias('mime_type')]
    property MimeType: String read FMimeType write FMimeType;
  End;

  /// <summary>This object represents one size of a photo or a file/sticker thumbnail.</summary>
  /// <remarks>A missing thumbnail for a file (or sticker) is presented as an empty object.</remarks>
  [Alias('PhotoSize')]
  TtgPhotoSize = Class(TtgFile)
  private
    FWidth: Integer;
    FHeight: Integer;
  published
    /// <summary>Photo width</summary>
    [Alias('width')]
    property Width: Integer read FWidth write FWidth;
    /// <summary>Photo height</summary>
    [Alias('Height')]
    property Height: Integer read FHeight write FHeight;
  End;

  /// <summary>This object represents a general file (as opposed to photos, voice messages and audio files).</summary>
  [Alias('Document')]
  TtgDocument = Class(TtgFile)
  private
    FThumb: TtgPhotoSize;
    FFileName: String;
    FMimeType: String;
  public
    destructor Destroy; override;
  published
    /// <summary>Document thumbnail as defined by sender</summary>
    [Alias('thumb')]
    property Thumb: TtgPhotoSize read FThumb write FThumb;
    /// <summary>Optional. Original filename as defined by sender</summary>
    [Alias('file_name')]
    property FileName: String read FFileName write FFileName;
    /// <summary>Optional. MIME type of the file as defined by sender</summary>
    [Alias('mime_type')]
    property MimeType: String read FMimeType write FMimeType;
  End;

  /// <summary>This object represents a sticker.</summary>
  [Alias('Sticker')]
  TtgSticker = Class(TtgFile)
  private
    FWidth: Integer;
    FHeight: Integer;
    FThumb: TtgPhotoSize;
    Femoji: String;
  public
    destructor Destroy; override;
  published
    /// <summary>Sticker width</summary>
    [Alias('width')]
    property Width: Integer read FWidth write FWidth;
    /// <summary>Sticker height</summary>
    [Alias('width')]
    property Height: Integer read FHeight write FHeight;
    /// <summary>Sticker thumbnail in .webp or .jpg format</summary>
    [Alias('thumb')]
    property Thumb: TtgPhotoSize read FThumb write FThumb;
    /// <summary>Optional. Emoji associated with the sticker</summary>
    [Alias('emoji')]
    property emoji: String read Femoji write Femoji;
  End;

  /// <summary>This object represents a video file.</summary>
  [Alias('Video')]
  TtgVideo = Class(TtgFile)
  private
    FWidth: Integer;
    FHeight: Integer;
    FDuration: Integer;
    FThumb: TtgPhotoSize;
    FMimeType: String;
  public
    destructor Destroy; override;
  published
    /// <summary>Video width as defined by sender</summary>
    [Alias('width')]
    property Width: Integer read FWidth write FWidth;
    /// <summary>Video height as defined by sender</summary>
    [Alias('height')]
    property Height: Integer read FHeight write FHeight;
    /// <summary>Duration of the video in seconds as defined by sender</summary>
    [Alias('duration')]
    property Duration: Integer read FDuration write FDuration;
    /// <summary>Video thumbnail</summary>
    [Alias('thumb')]
    property Thumb: TtgPhotoSize read FThumb write FThumb;
    /// <summary>Optional. Mime type of a file as defined by sender</summary>
    [Alias('mime_type')]
    property MimeType: String read FMimeType write FMimeType;
  End;

  /// <summary>This object represents a voice note.</summary>
  [Alias('Voice')]
  TtgVoice = Class(TtgFile)
  private
    FDuration: Integer;
    FMimeType: String;
  published
    /// <summary>Duration of the audio in seconds as defined by sender</summary>
    [Alias('duration')]
    property Duration: Integer read FDuration write FDuration;
    /// <summary>Optional. MIME type of the file as defined by sender</summary>
    [Alias('mime_type')]
    property MimeType: String read FMimeType write FMimeType;
  End;

  /// <summary>This object represents a phone contact.</summary>
  [Alias('Contact')]
  TtgContact = Class
  private
    FPhoneNumber: String;
    FFirstName: String;
    FLastName: String;
    FUserId: Integer;
  published
    /// <summary>Contact's phone number</summary>
    [Alias('phone_number')]
    property PhoneNumber: String read FPhoneNumber write FPhoneNumber;
    /// <summary>Contact's first name </summary>
    [Alias('first_name')]
    property FirstName: String read FFirstName write FFirstName;
    /// <summary>Optional. Contact's last name</summary>
    [Alias('last_name')]
    property LastName: String read FLastName write FLastName;
    /// <summary>Optional. Contact's user identifier in Telegram </summary>
    [Alias('user_id')]
    property UserId: Integer read FUserId write FUserId;
  End;

  /// <summary>This object represents a point on the map.</summary>
  [Alias('Location')]
  TtgLocation = Class
  private
    FLongitude: Single;
    FLatitude: Single;
  published
    /// <summary>Longitude as defined by sender</summary>
    [Alias('longitude')]
    property Longitude: Single read FLongitude write FLongitude;
    /// <summary>Latitude as defined by sender</summary>
    [Alias('latitude')]
    property Latitude: Single read FLatitude write FLatitude;
  End;

  /// <summary>This object represents a venue.</summary>
  [Alias('Venue')]
  TtgVenue = Class
  private
    FLocation: TtgLocation;
    Ftitle: String;
    FAddress: String;
    FFoursquareId: String;
  public
    destructor Destroy; override;
  published
    /// <summary>Venue location</summary>
    [Alias('location')]
    property Location: TtgLocation read FLocation write FLocation;
    /// <summary>Title of the result</summary>
    [Alias('title')]
    property title: String read Ftitle write Ftitle;
    /// <summary>Address of the venue </summary>
    [Alias('address')]
    property Address: String read FAddress write FAddress;
    /// <summary> Optional. Foursquare identifier of the venue</summary>
    [Alias('foursquare_id')]
    property FoursquareId: String read FFoursquareId write FFoursquareId;
  End;

  /// <summary>You can provide an animation for your game so that it looks stylish in chats (check out Lumberjack for an example). This object represents an animation file to be displayed in the message containing a game.</summary>
  [Alias('Animation')]
  TtgAnimation = Class
  private
    Ffile_id: String;
    Fthumb: TtgPhotoSize;
    Ffile_name: String;
    Fmime_type: String;
    Ffile_size: Integer;
  public
    destructor Destroy; override;
  published
    /// <summary>Unique file identifier</summary>
    [Alias('file_id')]
    property file_id: String read Ffile_id write Ffile_id;
    /// <summary>Optional. Animation thumbnail as defined by sender</summary>
    [Alias('thumb')]
    property thumb: TtgPhotoSize read Fthumb write Fthumb;
    /// <summary>Optional. Original animation filename as defined by sender</summary>
    [Alias('file_name')]
    property file_name: String read Ffile_name write Ffile_name;
    /// <summary>Optional. MIME type of the file as defined by sender</summary>
    [Alias('mime_type')]
    property mime_type: String read Fmime_type write Fmime_type;
    /// <summary>Optional. File size</summary>
    [Alias('file_size')]
    property file_size: Integer read Ffile_size write Ffile_size;
  End;
  /// <summary>
  ///   This object represents one row of the high scores table for a game.
  /// </summary>
  [Alias('Game')]
  TtgGameHighScore = Class
  private
    Fposition: Integer;
    Fuser: TtgUser;
    Fscore: Integer;
  public
    destructor Destroy; override;
  published
    /// <summary>
    ///   Position in high score table for the game
    /// </summary>
    property position: Integer read Fposition write Fposition;
    /// <summary>
    ///   User
    /// </summary>
    property user: TtgUser read Fuser write Fuser;
    /// <summary>
    ///   Score
    /// </summary>
    property score: Integer read Fscore write Fscore;
  End;
  /// <summary>This object represents a game. Use BotFather to create and edit games, their short names will act as unique identifiers.</summary>
  [Alias('Game')]
  TtgGame = Class
  private
    Ftitle: String;
    Fdescription: String;
    Fphoto: TArray<TtgPhotoSize>;
    Ftext: String;
    Ftext_entities: TArray<TtgMessageEntity>;
    Fanimation: TtgAnimation;
  public
    destructor Destroy; override;
  published
    /// <summary>Title of the game</summary>
    [Alias('title')]
    property title: String read Ftitle write Ftitle;
    /// <summary>Description of the game</summary>
    [Alias('description')]
    property description: String read Fdescription write Fdescription;
    /// <summary>Photo that will be displayed in the game message in chats.</summary>
    [Alias('photo')]
    property photo: TArray<TtgPhotoSize> read Fphoto write Fphoto;
    /// <summary>Optional. Brief description of the game or high scores included in the game message. Can be automatically edited to include current high scores for the game when the bot calls setGameScore, or manually edited using editMessageText. 0-4096 characters.</summary>
    [Alias('text')]
    property text: String read Ftext write Ftext;
    /// <summary>Optional. Special entities that appear in text, such as usernames, URLs, bot commands, etc.</summary>
    [Alias('text_entities')]
    property text_entities: TArray<TtgMessageEntity> read Ftext_entities write Ftext_entities;
    /// <summary>Optional. Animation that will be displayed in the game message in chats. Upload via BotFather</summary>
    [Alias('animation')]
    property animation: TtgAnimation read Fanimation write Fanimation;
  End;

  /// <summary>This object represents a message.</summary>
  [Alias('Message')]
  TtgMessage = Class
  private
    FMessageId: Integer;
    FFrom: TtgUser;
    FDate: Integer;
    FChat: TtgChat;
    FForwardFrom: TtgUser;
    FForwardDate: Integer;
    FReplyToMessage: TtgMessage;
    FText: String;
    FAudio: TtgAudio;
    FDocument: TtgDocument;
    FPhoto: TArray<TtgPhotoSize>;
    FSticker: TtgSticker;
    FVideo: TtgVideo;
    FVoice: TtgVoice;
    FCaption: String;
    FContact: TtgContact;
    FLocation: TtgLocation;
    FVenue: TtgVenue;
    FNewChatMember: TtgUser;
    FLeftChatMember: TtgUser;
    FNewChatTitle: String;
    FNewChatPhoto: TArray<TtgPhotoSize>;
    FDeleteChatPhoto: Boolean;
    FGroupChatCreated: Boolean;
    FSupergroupChatCreated: Boolean;
    FChannelChatCreated: Boolean;
    FMigrateToChatId: Int64;
    FMigrateFromChatId: Int64;
    FPinnedMessage: TtgMessage;
    Fentities: TArray<TtgMessageEntity>;
    Fforward_from_chat: TtgChat;
    FEditDate: Integer;
    Fgame: TtgGame;
  public
    destructor Destroy; override;
  published
    /// <summary>Unique message identifier</summary>
    [Alias('message_id')]
    property MessageId: Integer read FMessageId write FMessageId;
    /// <summary>Sender</summary>
    [Alias('from')]
    property From: TtgUser read FFrom write FFrom;
    /// <summary>Date the message was sent in Unix time</summary>
    [Alias('date')]
    property Date: Integer read FDate write FDate;
    /// <summary>Conversation the message belongs to</summary>
    [Alias('chat')]
    property Chat: TtgChat read FChat write FChat;
    /// <summary>Optional. For forwarded messages, sender of the original message</summary>
    [Alias('forward_from')]
    property ForwardFrom: TtgUser read FForwardFrom write FForwardFrom;
    /// <summary>Optional. For messages forwarded from a channel, information about the original channel</summary>
    [Alias('forward_from_chat')]
    property forward_from_chat: TtgChat read Fforward_from_chat write Fforward_from_chat;
    /// <summary>Optional. For forwarded messages, date the original message was sent in Unix time</summary>
    [Alias('forward_date')]
    property ForwardDate: Integer read FForwardDate write FForwardDate;
    /// <summary>Optional. For replies, the original message. Note that the Message object in this field will not contain further reply_to_message fields even if it itself is a reply.</summary>
    [Alias('reply_to_message')]
    property ReplyToMessage: TtgMessage read FReplyToMessage write FReplyToMessage;
    /// <summary>Optional. Date the message was last edited in Unix time.</summary>
    [Alias('edit_date')]
    property EditDate: Integer read FEditDate write FEditDate;
    /// <summary>Optional. For text messages, the actual UTF-8 text of the message</summary>
    [Alias('text')]
    property Text: String read FText write FText;
    /// <summary>Optional. For text messages, special entities like usernames, URLs, bot commands, etc. that appear in the text</summary>
    [Alias('entities')]
    property entities: TArray<TtgMessageEntity> read Fentities write Fentities;
    /// <summary>Optional. Message is an audio file, information about the file</summary>
    [Alias('audio')]
    property Audio: TtgAudio read FAudio write FAudio;
    /// <summary>Optional. Message is a general file, information about the file</summary>
    [Alias('document')]
    property Document: TtgDocument read FDocument write FDocument;
    /// <summary>Optional. Message is a game, information about the game. </summary>
    [Alias('game')]
    property game: TtgGame read Fgame write Fgame;
    /// <summary>Optional. Message is a photo, available sizes of the photo</summary>
    [Alias('photo')]
    property Photo: TArray<TtgPhotoSize> read FPhoto write FPhoto;
    /// <summary>Optional. Message is a sticker, information about the sticker</summary>
    [Alias('sticker')]
    property Sticker: TtgSticker read FSticker write FSticker;
    /// <summary>Optional. Message is a video, information about the video</summary>
    [Alias('video')]
    property Video: TtgVideo read FVideo write FVideo;
    /// <summary>Message is a voice message, information about the file</summary>
    [Alias('voice')]
    property Voice: TtgVoice read FVoice write FVoice;
    /// <summary>Optional. Caption for the document, photo or video, 0-200 characters</summary>
    [Alias('caption')]
    property Caption: String read FCaption write FCaption;
    /// <summary>Optional. Message is a shared contact, information about the contact</summary>
    [Alias('contact')]
    property Contact: TtgContact read FContact write FContact;
    /// <summary>Optional. Message is a shared location, information about the location</summary>
    [Alias('location')]
    property Location: TtgLocation read FLocation write FLocation;
    /// <summary>Optional. Message is a venue, information about the venue</summary>
    [Alias('venue')]
    property Venue: TtgVenue read FVenue write FVenue;
    /// <summary>Optional. A new member was added to the group, information about them (this member may be bot itself)</summary>
    [Alias('new_chat_member')]
    property NewChatMember: TtgUser read FNewChatMember write FNewChatMember;
    /// <summary>Optional. A member was removed from the group, information about them (this member may be bot itself)</summary>
    [Alias('left_chat_member')]
    property LeftChatMember: TtgUser read FLeftChatMember write FLeftChatMember;
    /// <summary>Optional. A group title was changed to this value</summary>
    [Alias('new_chat_title')]
    property NewChatTitle: String read FNewChatTitle write FNewChatTitle;
    /// <summary>Optional. A group photo was change to this value</summary>
    [Alias('new_chat_photo')]
    property NewChatPhoto: TArray<TtgPhotoSize> read FNewChatPhoto write FNewChatPhoto;
    /// <summary>Optional. Informs that the group photo was deleted</summary>
    [Alias('delete_chat_photo')]
    property DeleteChatPhoto: Boolean read FDeleteChatPhoto write FDeleteChatPhoto;
    /// <summary>Optional. Informs that the group has been created</summary>
    [Alias('group_chat_created')]
    property GroupChatCreated: Boolean read FGroupChatCreated write FGroupChatCreated;
    /// <summary>Optional. Service message: the supergroup has been created</summary>
    [Alias('supergroup_chat_created')]
    property SupergroupChatCreated: Boolean read FSupergroupChatCreated
      write FSupergroupChatCreated;
    /// <summary> Optional. Service message: the channel has been created </summary>
    [Alias('channel_chat_created')]
    property ChannelChatCreated: Boolean read FChannelChatCreated write FChannelChatCreated;
    /// <summary> Optional. The group has been migrated to a supergroup with the specified identifier</summary>
    [Alias('migrate_to_chat_id')]
    property MigrateToChatId: Int64 read FMigrateToChatId write FMigrateToChatId;
    /// <summary>Optional. The supergroup has been migrated from a group with the specified identifier</summary>
    [Alias('migrate_from_chat_id')]
    property MigrateFromChatId: Int64 read FMigrateFromChatId write FMigrateFromChatId;
    /// <summary>Optional. Specified message was pinned. Note that the Message object in this field will not contain further reply_to_message fields even if it is itself a reply</summary>
    [Alias('pinned_message')]
    property PinnedMessage: TtgMessage read FPinnedMessage write FPinnedMessage;
  End;

  /// <summary>This object represent a user's profile pictures.</summary>
  [Alias('UserProfilePhotos')]
  TtgUserProfilePhotos = Class
  private
    Ftotal_count: Integer;
    Fphotos: TArray<TArray<TtgPhotoSize>>;
  published
    /// <summary>Total number of profile pictures the target user has</summary>
    [Alias('total_count')]
    property total_count: Integer read Ftotal_count write Ftotal_count;
    /// <summary>Requested profile pictures (in up to 4 sizes each)</summary>
    [Alias('photos')]
    property photos: TArray < TArray < TtgPhotoSize >> read Fphotos write Fphotos;
  End;

  /// <summary>This object represents one button of the reply keyboard. For simple text buttons String can be used instead of this object to specify text of the button. Optional fields are mutually exclusive.</summary>
  /// <remarks>request_contact and request_location options will only work in Telegram versions released after 9 April, 2016. Older clients will ignore them.</remarks>
  [Alias('KeyboardButton')]
  TtgKeyboardButton = Class
  private
    FText: String;
    Frequest_contact: Boolean;
    Frequest_location: Boolean;
  protected
    function GetFullText: String; virtual;
  Public
    constructor Create(Const Text: String; request_contact: Boolean = False;
      request_location: Boolean = False); overload;
  published
    [Alias('text')]
    property FullText: String read GetFullText;
    /// <summary>Text of the button. If none of the optional fields are used, it will be sent to the bot as a message when the button is pressed</summary>
    [DISABLE]
    property Text: String read FText write FText;
    /// <summary>Optional. If True, the user's phone number will be sent as a contact when the button is pressed. Available in private chats only</summary>
    [Alias('request_contact')]
    property request_contact: Boolean read Frequest_contact write Frequest_contact;
    /// <summary>Optional. If True, the user's current location will be sent when the button is pressed. Available in private chats only</summary>
    [Alias('request_location')]
    property request_location: Boolean read Frequest_location write Frequest_location;
  End;

  TtgReplyMarkup = Class
  private
    Fselective: Boolean;
  published
    /// <summary>Optional. Use this parameter if you want to force reply from specific users only. Targets: 1) users that are @mentioned in the text of the Message object; 2) if the bot's message is a reply (has reply_to_message_id), sender of the original message.</summary>
    [Alias('selective')]
    property selective: Boolean read Fselective write Fselective;
  End;

  /// <summary>Upon receiving a message with this object, Telegram clients will display a reply interface to the user (act as if the user has selected the bot‘s message and tapped ’Reply'). This can be extremely useful if you want to create user-friendly step-by-step interfaces without having to sacrifice privacy mode. </summary>
  [Alias('ForceReply')]
  TtgForceReply = Class(TtgReplyMarkup)
  private
    Fforce_reply: Boolean;
  published
    /// <summary>Shows reply interface to the user, as if they manually selected the bot‘s message and tapped ’Reply'</summary>
    [Alias('force_reply')]
    property force_reply: Boolean read Fforce_reply write Fforce_reply;
  End;

  /// <summary>Upon receiving a message with this object, Telegram clients will hide the current custom keyboard and display the default letter-keyboard. By default, custom keyboards are displayed until a new keyboard is sent by a bot. An exception is made for one-time keyboards that are hidden immediately after the user presses a button (see ReplyKeyboardMarkup).</summary>
  [Alias('ReplyKeyboardHide')]
  TtgReplyKeyboardHide = Class(TtgReplyMarkup)
  private
    Fhide_keyboard: Boolean;
  published
    /// <summary>Requested profile pictures (in up to 4 sizes each)</summary>
    [Alias('hide_keyboard')]
    property hide_keyboard: Boolean read Fhide_keyboard write Fhide_keyboard;
  End;

  /// <summary>This object represents a custom keyboard with reply options (see Introduction to bots for details and examples).</summary>
  [Alias('ReplyKeyboardMarkup')]
  TtgReplyKeyboardMarkup = Class(TtgReplyMarkup)
  private
    Fresize_keyboard: Boolean;
    FKeyBoard: TArray<TArray<TtgKeyboardButton>>;
    Fone_time_keyboard: Boolean;
  published
    /// <summary>Array of button rows, each represented by an Array of KeyboardButton objects</summary>
    [Alias('keyboard')]
    property KeyBoard: TArray < TArray < TtgKeyboardButton >> read FKeyBoard write FKeyBoard;
    /// <summary>Optional. Requests clients to resize the keyboard vertically for optimal fit (e.g., make the keyboard smaller if there are just two rows of buttons). Defaults to false, in which case the custom keyboard is always of the same height as the app's standard keyboard.</summary>
    [Alias('resize_keyboard')]
    property resize_keyboard: Boolean read Fresize_keyboard write Fresize_keyboard;
    /// <summary>Optional. Requests clients to hide the keyboard as soon as it's been used. The keyboard will still be available, but clients will automatically display the usual letter-keyboard in the chat – the user can press a special button in the input field to see the custom keyboard again. Defaults to false.</summary>
    [Alias('one_time_keyboard')]
    property one_time_keyboard: Boolean read Fone_time_keyboard write Fone_time_keyboard;
  End;

  /// <summary>This object represents one button of an inline keyboard. You must use exactly one of the optional fields.</summary>
  [Alias('InlineKeyboardButton')]
  TtgInlineKeyboardButton = Class
  private
    FText: String;
    Furl: String;
    Fcallback_data: String;
    Fswitch_inline_query: String;
  protected
    Function GetFullText: String; virtual;
  published
    property FullText: String read GetFullText;
    /// <summary>Label text on the button</summary>
    // [DISABLE]
    [Alias('text')]
    property Text: String read FText write FText;
    /// <summary>Optional. HTTP url to be opened when button is pressed</summary>
    [Alias('url')]
    property url: String read Furl write Furl;
    /// <summary>Optional. Data to be sent in a callback query to the bot when button is pressed, 1-64 bytes</summary>
    [Alias('callback_data')]
    property callback_data: String read Fcallback_data write Fcallback_data;
    /// <summary>Optional. If set, pressing the button will prompt the user to select one of their chats, open that chat and insert the bot‘s username and the specified inline query in the input field. Can be empty, in which case just the bot’s username will be inserted.</summary>
    /// <remarks>Note: This offers an easy way for users to start using your bot in inline mode when they are currently in a private chat with it. Especially useful when combined with switch_pm… actions – in this case the user will be automatically returned to the chat they switched from, skipping the chat selection screen.</remarks>
    [Alias('switch_inline_query')]
    property switch_inline_query: String read Fswitch_inline_query write Fswitch_inline_query;
  End;

  /// <summary>This object represents an inline keyboard that appears right next to the message it belongs to.</summary>
  /// <remarks>Warning: Inline keyboards are currently being tested and are only available in one-on-one chats (i.e., user-bot or user-user in the case of inline bots).</remarks>
  [Alias('InlineKeyboardMarkup')]
  TtgInlineKeyboardMarkup = Class(TtgReplyMarkup)
  private
    Finline_keyboard: TArray<TArray<TtgInlineKeyboardButton>>;
  published
    /// <summary>Array of button rows, each represented by an Array of InlineKeyboardButton objects</summary>
    [Alias('inline_keyboard')]
    property inline_keyboard: TArray <TArray<TtgInlineKeyboardButton>> read Finline_keyboard write Finline_keyboard;
  End;

  [Alias('')]
  TtgApiResponse<T> = Class
  private
    FOk: Boolean;
    FResultObject: T;
    FMessage: String;
    FCode: Integer;
  public
  published
    /// <summary> Gets a value indicating whether the request was successful.</summary>
    [Alias('ok')]
    property Ok: Boolean read FOk write FOk;
    /// <summary>Gets the result object.</summary>
    /// <value>The result object.</value>
    [Alias('result')]
    property ResultObject: T read FResultObject write FResultObject;
    /// <summary>Gets the error message.</summary>
    /// <value>The error message.</value>
    [Alias('description')]
    property Message: String read FMessage write FMessage;
    /// <summary>Gets the error code.</summary>
    /// <value>The error code</value>
    [Alias('error_code')]
    property Code: Integer read FCode write FCode;
  End;

  [Alias('FileToSend')]
  TtgFileToSend = Class
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
  [Alias('InlineQuery')]
  TtgInlineQuery = Class
  private
    FID: String;
    FFrom: TtgUser;
    FQuery: String;
    Foffset: String;
  published
    /// <summary>Unique identifier for this query</summary>
    [Alias('id')]
    property ID: String read FID write FID;
    /// <summary>Sender</summary>
    [Alias('from')]
    property From: TtgUser read FFrom write FFrom;
    /// <summary>Text of the query</summary>
    [Alias('query')]
    property Query: String read FQuery write FQuery;
    /// <summary>Offset of the results to be returned, can be controlled by the bot</summary>
    [Alias('offset')]
    property offset: String read Foffset write Foffset;
  End;

  /// <summary>Represents a result of an inline query that was chosen by the user and sent to their chat partner.</summary>
  [Alias('ChosenInlineResult')]
  TtgChosenInlineResult = Class
  private
    FResultId: String;
    FFrom: TtgUser;
    FLocation: TtgLocation;
    FQuery: String;
    Finline_message_id: String;
  published
    /// <summary>The unique identifier for the result that was chosen.</summary>
    [Alias('result_id')]
    property ResultId: String read FResultId write FResultId;
    /// <summary>The user that chose the result.</summary>
    [Alias('from')]
    property From: TtgUser read FFrom write FFrom;
    /// <summary>The query that was used to obtain the result.</summary>
    [Alias('location')]
    property Location: TtgLocation read FLocation write FLocation;
    /// <summary>Optional. Identifier of the sent inline message. Available only if there is an inline keyboard attached to the message. Will be also received in callback queries and can be used to edit the message.</summary>
    [Alias('inline_message_id')]
    property inline_message_id: String read Finline_message_id write Finline_message_id;
    /// <summary>The query that was used to obtain the result.</summary>
    [Alias('query')]
    property Query: String read FQuery write FQuery;
  End;

  [Alias('CallbackQuery')]
  TtgCallbackQuery = Class
  private
    FID: String;
    FFrom: TtgUser;
    FMessage: TtgMessage;
    FInlineMessageId: String;
    FData: String;
  published
    /// <summary>Unique identifier for this query</summary>
    [Alias('id')]
    property ID: String read FID write FID;
    /// <summary>Sender</summary>
    [Alias('from')]
    property From: TtgUser read FFrom write FFrom;
    /// <summary>Optional. Message with the callback button that originated the query. Note that message content and message date will not be available if the message is too old</summary>
    [Alias('message')]
    property Message: TtgMessage read FMessage write FMessage;
    /// <summary>Optional. Identifier of the message sent via the bot in inline mode, that originated the query</summary>
    [Alias('inline_message_id')]
    property InlineMessageId: String read FInlineMessageId write FInlineMessageId;
    /// <summary>Data associated with the callback button. Be aware that a bad client can send arbitrary data in this field</summary>
    [Alias('data')]
    property Data: String read FData write FData;
  End;

  /// <summary>=This object represents an incoming update.</summary>
  /// <remarks>Only one of the optional parameters can be present in any given update.</remarks>
  [Alias('Update')]
  TtgUpdate = Class
  private
    FID: Integer;
    FMessage: TtgMessage;
    FInlineQuery: TtgInlineQuery;
    FChosenInlineResult: TtgChosenInlineResult;
    FCallbackQuery: TtgCallbackQuery;
    FEditedMessage: TtgMessage;
    Fchannel_post: TtgMessage;
    Fedited_channel_post: TtgMessage;
  public
    destructor Destroy; override;
  published
    /// <summary>The update‘s unique identifier. Update identifiers start from a certain positive number and increase sequentially. This ID becomes especially handy if you’re using Webhooks, since it allows you to ignore repeated updates or to restore the correct update sequence, should they get out of order. </summary>
    [Alias('update_id')]
    property ID: Integer read FID write FID;
    /// <summary>Optional. New incoming message of any kind — text, photo, sticker, etc.</summary>
    [Alias('message')]
    property Message: TtgMessage read FMessage write FMessage;
    /// <summary>Optional. New version of a message that is known to the bot and was edited</summary>
    [Alias('edited_message')]
    property EditedMessage: TtgMessage read FEditedMessage write FEditedMessage;
    /// <summary>Optional. New incoming channel post of any kind — text, photo, sticker, etc.</summary>
    [Alias('channel_post')]
    property channel_post: TtgMessage read Fchannel_post write Fchannel_post;
    /// <summary>Optional. New version of a channel post that is known to the bot and was edited</summary>
    [Alias('edited_channel_post')]
    property edited_channel_post: TtgMessage read Fedited_channel_post write Fedited_channel_post;
    /// <summary>Optional. New incoming inline query</summary>
    [Alias('inline_query')]
    property InlineQuery: TtgInlineQuery read FInlineQuery write FInlineQuery;
    /// <summary>Optional. The result of a inline query that was chosen by a user and sent to their chat partner</summary>
    [Alias('chosen_inline_result')]
    property ChosenInlineResult: TtgChosenInlineResult read FChosenInlineResult
      write FChosenInlineResult;
    /// <summary>Optional. New incoming callback query</summary>
    [Alias('callback_query')]
    property CallbackQuery: TtgCallbackQuery read FCallbackQuery write FCallbackQuery;
  End;

  /// <summary>This object represents the content of a message to be sent as a result of an inline query.</summary>
  [Alias('InputMessageContent')]
  TtgInputMessageContent = Class

  End;

  /// <summary>Represents the content of a text message to be sent as the result of an inline query.</summary>
  [Alias('InputTextMessageContent')]
  TtgInputTextMessageContent = Class(TtgInputMessageContent)
  private
    Fmessage_text: String;
    Fparse_mode: String;
    Fdisable_web_page_preview: Boolean;
  published
    /// <summary>Text of the message to be sent, 1-4096 characters</summary>
    [Alias('message_text')]
    property message_text: String read Fmessage_text write Fmessage_text;
    /// <summary>Optional. Send Markdown or HTML, if you want Telegram apps to show bold, italic, fixed-width text or inline URLs in your bot's message.</summary>
    [Alias('parse_mode')]
    property parse_mode: String read Fparse_mode write Fparse_mode;
    /// <summary>Optional. Disables link previews for links in the sent message</summary>
    [Alias('disable_web_page_preview')]
    property disable_web_page_preview: Boolean read Fdisable_web_page_preview write Fdisable_web_page_preview;
  End;

  /// <summary>Represents the content of a location message to be sent as the result of an inline query.</summary>
  [Alias('InputLocationMessageContent')]
  TtgInputLocationMessageContent = Class(TtgInputMessageContent)
  private
    FLatitude: Single;
    FLongitude: Single;
  published
    /// <summary>Latitude of the location in degrees</summary>
    [Alias('latitude')]
    property Latitude: Single read FLatitude write FLatitude;
    /// <summary>Longitude of the location in degrees</summary>
    [Alias('longitude')]
    property Longitude: Single read FLongitude write FLongitude;
  End;

  /// <summary>Represents the content of a venue message to be sent as the result of an inline query.</summary>
  [Alias('InputVenueMessageContent')]
  TtgInputVenueMessageContent = Class(TtgInputMessageContent)
  private
    FLatitude: Single;
    FLongitude: Single;
    Ftitle: String;
    FAddress: String;
    Ffoursquare_id: String;
  published
    /// <summary>Latitude of the venue in degrees</summary>
    [Alias('latitude')]
    property Latitude: Single read FLatitude write FLatitude;
    /// <summary>Longitude of the venue in degrees</summary>
    [Alias('longitude')]
    property Longitude: Single read FLongitude write FLongitude;
    /// <summary>Name of the venue</summary>
    [Alias('title')]
    property title: String read Ftitle write Ftitle;
    /// <summary>Address of the venue</summary>
    [Alias('address')]
    property Address: String read FAddress write FAddress;
    /// <summary>Optional. Foursquare identifier of the venue, if known</summary>
    [Alias('foursquare_id')]
    property foursquare_id: String read Ffoursquare_id write Ffoursquare_id;
  End;

  /// <summary>Represents the content of a contact message to be sent as the result of an inline query.</summary>
  [Alias('InputContactMessageContent')]
  TtgInputContactMessageContent = Class(TtgInputMessageContent)
  private
    Fphone_number: String;
    Ffirst_name: String;
    Flast_name: String;
  published
    /// <summary>Contact's phone number</summary>
    [Alias('phone_number')]
    property phone_number: String read Fphone_number write Fphone_number;
    /// <summary>Contact's first name</summary>
    [Alias('first_name')]
    property first_name: String read Ffirst_name write Ffirst_name;
    /// <summary>Optional. Contact's last name</summary>
    [Alias('last_name')]
    property last_name: String read Flast_name write Flast_name;
  End;

  /// <summary></summary>
  [Alias('')]
  /// <summary>This object represents one result of an inline query. Telegram clients currently support results of the following 19 types   /// </summary>
  [Alias('InlineQueryResult')]
  TtgInlineQueryResult = Class
  private
    Ftype: String;
    FID: String;
    Freply_markup: TtgInlineKeyboardMarkup;
  public
    destructor Destroy; override;
  published
    /// <summary>Type of the result</summary>
    [Alias('type')]
    property &type: String read Ftype write Ftype;
    /// <summary>Unique identifier for this result, 1-64 bytes</summary>
    [Alias('id')]
    property ID: String read FID write FID;
    /// <summary>Optional. Inline keyboard attached to the message</summary>
    [Alias('reply_markup')]
    property reply_markup: TtgInlineKeyboardMarkup read Freply_markup write Freply_markup;
    /// <summary>Optional. Inline keyboard attached to the message</summary>
    [Alias('reply_markup')]
    property input_message_content: TtgInlineKeyboardMarkup read Freply_markup write Freply_markup;
  End;

  /// <summary>Represents a link to an article or web page.</summary>
  [Alias('InlineQueryResultArticle')]
  TtgInlineQueryResultArticle = class(TtgInlineQueryResult)
  private
    Ftitle: String;
    Furl: String;
    Fhide_url: Boolean;
    Fdescription: String;
    Fthumb_url: String;
    Fthumb_width: Integer;
    Fthumb_height: Integer;
  published
    /// <summary>Title of the result</summary>
    [Alias('title')]
    property title: String read Ftitle write Ftitle;
    /// <summary>Optional. URL of the result</summary>
    [Alias('url')]
    property url: String read Furl write Furl;
    /// <summary>Optional. Pass True, if you don't want the URL to be shown in the message</summary>
    [Alias('hide_url')]
    property hide_url: Boolean read Fhide_url write Fhide_url;
    /// <summary>Optional. Short description of the result</summary>
    [Alias('description')]
    property description: String read Fdescription write Fdescription;
    /// <summary>Optional. Url of the thumbnail for the result</summary>
    [Alias('thumb_url')]
    property thumb_url: String read Fthumb_url write Fthumb_url;
    /// <summary>Optional. Thumbnail width</summary>
    [Alias('thumb_width')]
    property thumb_width: Integer read Fthumb_width write Fthumb_width;
    /// <summary>Optional. Thumbnail height</summary>
    [Alias('thumb_height')]
    property thumb_height: Integer read Fthumb_height write Fthumb_height;
  end;

  /// <summary>Represents a link to a photo. By default, this photo will be sent by the user with optional caption. Alternatively, you can use input_message_content to send a message with the specified content instead of the photo.</summary>
  [Alias('InlineQueryResultPhoto')]
  TtgInlineQueryResultPhoto = Class(TtgInlineQueryResult)
  private
    Fphoto_url: String;
    Fthumb_url: String;
    Fphoto_width: Integer;
    Fphoto_height: Integer;
    Ftitle: String;
    Fdescription: String;
    FCaption: String;
  published
    /// <summary>A valid URL of the photo. Photo must be in jpeg format. Photo size must not exceed 5MB</summary>
    [Alias('photo_url')]
    property photo_url: String read Fphoto_url write Fphoto_url;
    /// <summary>URL of the thumbnail for the photo</summary>
    [Alias('thumb_url')]
    property thumb_url: String read Fthumb_url write Fthumb_url;
    /// <summary>Optional. Width of the photo</summary>
    [Alias('photo_width')]
    property photo_width: Integer read Fphoto_width write Fphoto_width;
    /// <summary>Optional. Height of the photo</summary>
    [Alias('photo_height')]
    property photo_height: Integer read Fphoto_height write Fphoto_height;
    /// <summary>Optional. Title for the result</summary>
    [Alias('title')]
    property title: String read Ftitle write Ftitle;
    /// <summary>	Optional. Short description of the result</summary>
    [Alias('description')]
    property description: String read Fdescription write Fdescription;
    /// <summary>Optional. Caption of the photo to be sent, 0-200 characters</summary>
    [Alias('caption')]
    property Caption: String read FCaption write FCaption;
  End;

  /// <summary>Represents a link to an animated GIF file. By default, this animated GIF file will be sent by the user with optional caption. Alternatively, you can use input_message_content to send a message with the specified content instead of the animation.</summary>
  [Alias('InlineQueryResultGif')]
  TtgInlineQueryResultGif = Class(TtgInlineQueryResult)
  private
    Fgif_url: String;
    Fgif_width: Integer;
    Fgif_height: Integer;
    Fthumb_url: String;
    Ftitle: String;
    FCaption: String;
  published
    /// <summary>A valid URL for the GIF file. File size must not exceed 1MB</summary>
    [Alias('gif_url')]
    property gif_url: String read Fgif_url write Fgif_url;
    /// <summary>Optional. Width of the GIF</summary>
    [Alias('gif_width')]
    property gif_width: Integer read Fgif_width write Fgif_width;
    /// <summary>Optional. Height of the GIF</summary>
    [Alias('gif_height')]
    property gif_height: Integer read Fgif_height write Fgif_height;
    /// <summary>URL of the static thumbnail for the result (jpeg or gif)</summary>
    [Alias('thumb_url')]
    property thumb_url: String read Fthumb_url write Fthumb_url;
    /// <summary>Optional. Title for the result</summary>
    [Alias('title')]
    property title: String read Ftitle write Ftitle;
    /// <summary>Optional. Caption of the GIF file to be sent, 0-200 characters</summary>
    [Alias('caption')]
    property Caption: String read FCaption write FCaption;
  End;

  [Alias('InlineQueryResultMpeg4Gif')]
  /// <summary>Represents a link to a video animation (H.264/MPEG-4 AVC video without sound). By default, this animated MPEG-4 file will be sent by the user with optional caption. Alternatively, you can use input_message_content to send a message with the specified content instead of the animation.</summary>
  TtgInlineQueryResultMpeg4Gif = Class(TtgInlineQueryResult)
  private
    Fmpeg4_url: String;
    Fmpeg4_width: Integer;
    Fmpeg4_height: Integer;
    Fthumb_url: String;
    Ftitle: String;
    FCaption: string;
  published
    /// <summary>A valid URL for the MP4 file. File size must not exceed 1MB</summary>
    [Alias('mpeg4_url')]
    property mpeg4_url: String read Fmpeg4_url write Fmpeg4_url;
    /// <summary>Optional. Video width</summary>
    [Alias('mpeg4_width')]
    property mpeg4_width: Integer read Fmpeg4_width write Fmpeg4_width;
    /// <summary>Optional. Video height</summary>
    [Alias('mpeg4_height')]
    property mpeg4_height: Integer read Fmpeg4_height write Fmpeg4_height;
    /// <summary>URL of the static thumbnail (jpeg or gif) for the result</summary>
    [Alias('thumb_url')]
    property thumb_url: String read Fthumb_url write Fthumb_url;
    /// <summary>Optional. Title for the result</summary>
    [Alias('title')]
    property title: String read Ftitle write Ftitle;
    /// <summary>Optional. Caption of the MPEG-4 file to be sent, 0-200 characters</summary>
    [Alias('caption')]
    property Caption: string read FCaption write FCaption;

  End;

  /// <summary>Represents a link to a page containing an embedded video player or a video file. By default, this video file will be sent by the user with an optional caption. Alternatively, you can use input_message_content to send a message with the specified content instead of the video.</summary>
  [Alias('InlineQueryResultVideo')]
  TtgInlineQueryResultVideo = Class(TtgInlineQueryResult)
  private
    Fvideo_url: String;
    Fmime_type: String;
    Fthumb_url: String;
    Ftitle: String;
    FCaption: String;
    Fvideo_width: Integer;
    Fvideo_height: Integer;
    Fvideo_duration: Integer;
    Fdescription: String;
  published
    /// <summary>A valid URL for the embedded video player or video file</summary>
    [Alias('video_url')]
    property video_url: String read Fvideo_url write Fvideo_url;
    /// <summary>Mime type of the content of video url, “text/html” or “video/mp4”</summary>
    [Alias('mime_type')]
    property mime_type: String read Fmime_type write Fmime_type;
    /// <summary>URL of the thumbnail (jpeg only) for the video</summary>
    [Alias('thumb_url')]
    property thumb_url: String read Fthumb_url write Fthumb_url;
    /// <summary>Title for the result</summary>
    [Alias('title')]
    property title: String read Ftitle write Ftitle;
    /// <summary>Optional. Caption of the video to be sent, 0-200 characters</summary>
    [Alias('caption')]
    property Caption: String read FCaption write FCaption;
    /// <summary>Optional. Video width</summary>
    [Alias('video_width')]
    property video_width: Integer read Fvideo_width write Fvideo_width;
    /// <summary>Optional. Video height</summary>
    [Alias('video_height')]
    property video_height: Integer read Fvideo_height write Fvideo_height;
    /// <summary>Optional. Video duration in seconds</summary>
    [Alias('video_duration')]
    property video_duration: Integer read Fvideo_duration write Fvideo_duration;
    /// <summary>Optional. Short description of the result</summary>
    [Alias('description')]
    property description: String read Fdescription write Fdescription;
  End;

  /// <summary>Represents a link to an mp3 audio file. By default, this audio file will be sent by the user. Alternatively, you can use input_message_content to send a message with the specified content instead of the audio. </summary>
  [Alias('InlineQueryResultAudio')]
  TtgInlineQueryResultAudio = class(TtgInlineQueryResult)
  private
    Faudio_url: String;
    Ftitle: String;
    FPerformer: String;
    Faudio_duration: Integer;
  published
    /// <summary>A valid URL for the audio file</summary>
    [Alias('audio_url')]
    property audio_url: String read Faudio_url write Faudio_url;
    /// <summary>Title</summary>
    [Alias('title')]
    property title: String read Ftitle write Ftitle;
    /// <summary>Optional. Performer</summary>
    [Alias('performer')]
    property Performer: String read FPerformer write FPerformer;
    /// <summary>Optional. Audio duration in seconds</summary>
    [Alias('audio_duration')]
    property audio_duration: Integer read Faudio_duration write Faudio_duration;
  end;

  /// <summary>Represents a link to a voice recording in an .ogg container encoded with OPUS. By default, this voice recording will be sent by the user. Alternatively, you can use input_message_content to send a message with the specified content instead of the the voice message.</summary>
  [Alias('InlineQueryResultVoice')]
  TtgInlineQueryResultVoice = Class(TtgInlineQueryResult)
  private
    Fvoice_url: String;
    Ftitle: String;
    Fvoice_duration: Integer;
  published
    /// <summary>A valid URL for the voice recording</summary>
    [Alias('voice_url')]
    property voice_url: String read Fvoice_url write Fvoice_url;
    /// <summary>Recording title</summary>
    [Alias('title')]
    property title: String read Ftitle write Ftitle;
    /// <summary>Optional. Recording duration in seconds</summary>
    [Alias('voice_duration')]
    property voice_duration: Integer read Fvoice_duration write Fvoice_duration;
  End;

  /// <summary>Represents a link to a file. By default, this file will be sent by the user with an optional caption. Alternatively, you can use input_message_content to send a message with the specified content instead of the file. Currently, only .PDF and .ZIP files can be sent using this method.</summary>
  [Alias('InlineQueryResultDocument')]
  TtgInlineQueryResultDocument = Class(TtgInlineQueryResult)
  private
    Ftitle: String;
    FCaption: String;
    Fdocument_url: String;
    Fmime_type: String;
    Fdescription: String;
    Fthumb_url: String;
    Fthumb_width: Integer;
    Fthumb_height: Integer;
  published
    /// <summary>Title for the result</summary>
    [Alias('title')]
    property title: String read Ftitle write Ftitle;
    /// <summary>Optional. Caption of the document to be sent, 0-200 characters</summary>
    [Alias('caption')]
    property Caption: String read FCaption write FCaption;
    /// <summary>A valid URL for the file</summary>
    [Alias('document_url')]
    property document_url: String read Fdocument_url write Fdocument_url;
    /// <summary>Mime type of the content of the file, either “application/pdf” or “application/zip”</summary>
    [Alias('mime_type')]
    property mime_type: String read Fmime_type write Fmime_type;
    /// <summary>Optional. Short description of the result</summary>
    [Alias('description')]
    property description: String read Fdescription write Fdescription;
    /// <summary>Optional. URL of the thumbnail (jpeg only) for the file</summary>
    [Alias('thumb_url')]
    property thumb_url: String read Fthumb_url write Fthumb_url;
    /// <summary>Optional. Thumbnail width</summary>
    [Alias('thumb_width')]
    property thumb_width: Integer read Fthumb_width write Fthumb_width;
    /// <summary>Optional. Thumbnail height</summary>
    [Alias('thumb_height')]
    property thumb_height: Integer read Fthumb_height write Fthumb_height;
  End;

  /// <summary>Represents a location on a map. By default, the location will be sent by the user. Alternatively, you can use input_message_content to send a message with the specified content instead of the location.</summary>
  [Alias('InlineQueryResultLocation')]
  TtgInlineQueryResultLocation = Class(TtgInlineQueryResult)
  private
    FLatitude: Single;
    FLongitude: Single;
    Ftitle: String;
    Fthumb_url: String;
    Fthumb_width: Integer;
    Fthumb_height: Integer;
  published
    /// <summary>Location latitude in degrees</summary>
    [Alias('latitude')]
    property Latitude: Single read FLatitude write FLatitude;
    /// <summary>Location longitude in degrees</summary>
    [Alias('longitude')]
    property Longitude: Single read FLongitude write FLongitude;
    /// <summary>Location title</summary>
    [Alias('title')]
    property title: String read Ftitle write Ftitle;
    /// <summary>Optional. Url of the thumbnail for the result</summary>
    [Alias('thumb_url')]
    property thumb_url: String read Fthumb_url write Fthumb_url;
    /// <summary>Optional. Thumbnail width</summary>
    [Alias('thumb_width')]
    property thumb_width: Integer read Fthumb_width write Fthumb_width;
    /// <summary>Optional. Thumbnail height</summary>
    [Alias('thumb_height')]
    property thumb_height: Integer read Fthumb_height write Fthumb_height;
  End;

  /// <summary>Represents a venue. By default, the venue will be sent by the user. Alternatively, you can use input_message_content to send a message with the specified content instead of the venue.</summary>
  [Alias('InlineQueryResultVenue')]
  TtgInlineQueryResultVenue = Class(TtgInlineQueryResult)
  private
    FLatitude: Single;
    Ftitle: String;
    FLongitude: Single;
    Fthumb_width: Integer;
    Fthumb_url: String;
    Fthumb_height: Integer;
    FAddress: String;
    Ffoursquare_id: String;
  published
    /// <summary>Latitude of the venue location in degrees</summary>
    [Alias('latitude')]
    property Latitude: Single read FLatitude write FLatitude;
    /// <summary>Longitude of the venue location in degrees</summary>
    [Alias('longitude')]
    property Longitude: Single read FLongitude write FLongitude;
    /// <summary>Title of the venue</summary>
    [Alias('title')]
    property title: String read Ftitle write Ftitle;
    /// <summary>Address of the venue</summary>
    [Alias('address')]
    property Address: String read FAddress write FAddress;
    /// <summary>Optional. Foursquare identifier of the venue if known</summary>
    [Alias('foursquare_id')]
    property foursquare_id: String read Ffoursquare_id write Ffoursquare_id;
    /// <summary>Optional. Url of the thumbnail for the result</summary>
    [Alias('thumb_url')]
    property thumb_url: String read Fthumb_url write Fthumb_url;
    /// <summary>Optional. Thumbnail width</summary>
    [Alias('thumb_width')]
    property thumb_width: Integer read Fthumb_width write Fthumb_width;
    /// <summary>Optional. Thumbnail height</summary>
    [Alias('thumb_height')]
    property thumb_height: Integer read Fthumb_height write Fthumb_height;
  End;

  /// <summary>Represents a contact with a phone number. By default, this contact will be sent by the user. Alternatively, you can use input_message_content to send a message with the specified content instead of the contact.</summary>
  [Alias('InlineQueryResultContact')]
  TtgInlineQueryResultContact = Class
  private
    Fthumb_width: Integer;
    Fthumb_url: String;
    Fthumb_height: Integer;
    Fphone_number: String;
    Ffirst_name: String;
    Flast_name: String;
  published
    /// <summary>Contact's phone number</summary>
    [Alias('phone_number')]
    property phone_number: String read Fphone_number write Fphone_number;
    /// <summary>Contact's first name</summary>
    [Alias('first_name')]
    property first_name: String read Ffirst_name write Ffirst_name;
    /// <summary>Optional. Contact's last name</summary>
    [Alias('last_name')]
    property last_name: String read Flast_name write Flast_name;
    /// <summary>Optional. Url of the thumbnail for the result</summary>
    [Alias('thumb_url')]
    property thumb_url: String read Fthumb_url write Fthumb_url;
    /// <summary>Optional. Thumbnail width</summary>
    [Alias('thumb_width')]
    property thumb_width: Integer read Fthumb_width write Fthumb_width;
    /// <summary>Optional. Thumbnail height</summary>
    [Alias('thumb_height')]
    property thumb_height: Integer read Fthumb_height write Fthumb_height;
  End;

  TtgInlineQueryResultCached = Class(TtgInlineQueryResult)
  End;

  /// <summary>Represents a link to a photo stored on the Telegram servers. By default, this photo will be sent by the user with an optional caption. Alternatively, you can use input_message_content to send a message with the specified content instead of the photo.</summary>
  [Alias('InlineQueryResultCachedPhoto')]
  TtgInlineQueryResultCachedPhoto = Class(TtgInlineQueryResultCached)
  private
    Fphoto_file_id: String;
    Ftitle: String;
    Fdescription: String;
    FCaption: String;
  published
    /// <summary>A valid file identifier of the photo</summary>
    [Alias('photo_file_id')]
    property photo_file_id: String read Fphoto_file_id write Fphoto_file_id;
    /// <summary>Optional. Title for the result</summary>
    [Alias('title')]
    property title: String read Ftitle write Ftitle;
    /// <summary>Optional. Short description of the result</summary>
    [Alias('description')]
    property description: String read Fdescription write Fdescription;
    /// <summary>Optional. Caption of the photo to be sent, 0-200 characters</summary>
    [Alias('caption')]
    property Caption: String read FCaption write FCaption;
  End;

  /// <summary>Represents a link to an animated GIF file stored on the Telegram servers. By default, this animated GIF file will be sent by the user with an optional caption. Alternatively, you can use input_message_content to send a message with specified content instead of the animation.</summary>
  [Alias('InlineQueryResultCachedGif')]
  TtgInlineQueryResultCachedGif = Class(TtgInlineQueryResultCached)
  private
    Fgif_file_id: String;
    Ftitle: String;
    FCaption: String;
  published
    /// <summary>A valid file identifier for the GIF file</summary>
    [Alias('gif_file_id')]
    property gif_file_id: String read Fgif_file_id write Fgif_file_id;
    /// <summary>Optional. Title for the result</summary>
    [Alias('title')]
    property title: String read Ftitle write Ftitle;
    /// <summary>Optional. Caption of the GIF file to be sent, 0-200 characters</summary>
    [Alias('caption')]
    property Caption: String read FCaption write FCaption;
  End;

  /// <summary>Represents a link to a video animation (H.264/MPEG-4 AVC video without sound) stored on the Telegram servers. By default, this animated MPEG-4 file will be sent by the user with an optional caption. Alternatively, you can use input_message_content to send a message with the specified content instead of the animation.</summary>
  [Alias('InlineQueryResultCachedMpeg4Gif')]
  TtgInlineQueryResultCachedMpeg4Gif = Class(TtgInlineQueryResultCached)
  private
    Fmpeg4_file_id: String;
    Ftitle: String;
    FCaption: String;
  published
    /// <summary>A valid file identifier for the MP4 file</summary>
    [Alias('mpeg4_file_id')]
    property mpeg4_file_id: String read Fmpeg4_file_id write Fmpeg4_file_id;
    /// <summary>Optional. Title for the result</summary>
    [Alias('title')]
    property title: String read Ftitle write Ftitle;
    /// <summary>Optional. Caption of the MPEG-4 file to be sent, 0-200 characters</summary>
    [Alias('caption')]
    property Caption: String read FCaption write FCaption;
  End;

  /// <summary>Represents a link to a sticker stored on the Telegram servers. By default, this sticker will be sent by the user. Alternatively, you can use input_message_content to send a message with the specified content instead of the sticker.</summary>
  [Alias('InlineQueryResultCachedSticker')]
  TtgInlineQueryResultCachedSticker = Class(TtgInlineQueryResultCached)
  private
    Fsticker_file_id: String;
  published
    /// <summary>A valid file identifier of the sticker</summary>
    [Alias('sticker_file_id')]
    property sticker_file_id: String read Fsticker_file_id write Fsticker_file_id;
  End;

  /// <summary>Represents a link to a file stored on the Telegram servers. By default, this file will be sent by the user with an optional caption. Alternatively, you can use input_message_content to send a message with the specified content instead of the file. Currently, only pdf-files and zip archives can be sent using this method.</summary>
  [Alias('InlineQueryResultCachedDocument')]
  TTgInlineQueryResultCachedDocument = Class(TtgInlineQueryResultCached)
  private
    Ftitle: String;
    Fdocument_file_id: String;
    Fdescription: String;
    FCaption: String;
  published
    /// <summary>Title for the result</summary>
    [Alias('title')]
    property title: String read Ftitle write Ftitle;
    /// <summary>A valid file identifier for the file</summary>
    [Alias('document_file_id')]
    property document_file_id: String read Fdocument_file_id write Fdocument_file_id;
    /// <summary>Optional. Short description of the result</summary>
    [Alias('description')]
    property description: String read Fdescription write Fdescription;
    /// <summary>Optional. Caption of the document to be sent, 0-200 characters</summary>
    [Alias('caption')]
    property Caption: String read FCaption write FCaption;
  End;

  /// <summary>Represents a link to a video file stored on the Telegram servers. By default, this video file will be sent by the user with an optional caption. Alternatively, you can use input_message_content to send a message with the specified content instead of the video.</summary>
  [Alias('InlineQueryResultCachedVideo')]
  TtgInlineQueryResultCachedVideo = Class(TtgInlineQueryResultCached)
  private
    FCaption: String;
    Fdescription: String;
    Fvideo_file_id: String;
    Ftitle: String;
  published
    /// <summary>Title for the result</summary>
    [Alias('title')]
    property title: String read Ftitle write Ftitle;
    /// <summary>A valid file identifier for the video</summary>
    [Alias('video_file_id')]
    property video_file_id: String read Fvideo_file_id write Fvideo_file_id;
    /// <summary>Optional. Short description of the result</summary>
    [Alias('description')]
    property description: String read Fdescription write Fdescription;
    /// <summary>Optional. Caption of the video to be sent, 0-200 characters</summary>
    [Alias('caption')]
    property Caption: String read FCaption write FCaption;
  End;

  /// <summary>Represents a link to a voice message stored on the Telegram servers. By default, this voice message will be sent by the user. Alternatively, you can use input_message_content to send a message with the specified content instead of the voice message.</summary>
  [Alias('InlineQueryResultCachedVoice')]
  TtgInlineQueryResultCachedVoice = Class(TtgInlineQueryResultCached)
  private
    Fvoice_file_id: String;
    Ftitle: String;
  published
    /// <summary>A valid file identifier for the voice message</summary>
    [Alias('voice_file_id')]
    property voice_file_id: String read Fvoice_file_id write Fvoice_file_id;
    /// <summary>Voice message title</summary>
    [Alias('title')]
    property title: String read Ftitle write Ftitle;
  End;

  /// <summary>Represents a link to an mp3 audio file stored on the Telegram servers. By default, this audio file will be sent by the user. Alternatively, you can use input_message_content to send a message with the specified content instead of the audio.</summary>
  [Alias('InlineQueryResultCachedAudio')]
  TtgInlineQueryResultCachedAudio = Class(TtgInlineQueryResultCached)
  private
    Faudio_file_id: String;
  published
    /// <summary>A valid file identifier for the audio file</summary>
    [Alias('audio_file_id')]
    property audio_file_id: String read Faudio_file_id write Faudio_file_id;
  End;

  /// <summary>
  ///   Contains information about the current status of a webhook.
  /// </summary>
  TtgWebhookInfo = Class
  private
    Furl: String;
    Fhas_custom_certificate: Boolean;
    Fpending_update_count: Integer;
    Flast_error_date: Integer;
    Flast_error_message: String;
    Fmax_connections: Integer;
    Fallowed_updates: TArray<String>;
  published
    /// <summary>
    ///   Webhook URL, may be empty if webhook is not set up
    /// </summary>
    [Alias('url')]
    property url: String read Furl write Furl;
    /// <summary>
    ///   True, if a custom certificate was provided for webhook certificate
    ///   checks
    /// </summary>
    [Alias('has_custom_certificate')]
    property has_custom_certificate: Boolean read Fhas_custom_certificate write Fhas_custom_certificate;
    /// <summary>
    ///   Number of updates awaiting delivery
    /// </summary>
    [Alias('pending_update_count')]
    property pending_update_count: Integer read Fpending_update_count write Fpending_update_count;
    /// <summary>
    ///   Optional. Unix time for the most recent error that happened when
    ///   trying to deliver an update via webhook
    /// </summary>
    [Alias('last_error_date')]
    property last_error_date: Integer read Flast_error_date write Flast_error_date;
    /// <summary>
    ///   Optional. Error message in human-readable format for the most recent
    ///   error that happened when trying to deliver an update via webhook
    /// </summary>
    [Alias('last_error_message')]
    property last_error_message: String read Flast_error_message write Flast_error_message;
    /// <summary>
    ///   Optional. Maximum allowed number of simultaneous HTTPS connections to
    ///   the webhook for update delivery
    /// </summary>
    [Alias('max_connections')]
    property max_connections: Integer read Fmax_connections write Fmax_connections;
    /// <summary>
    ///   Optional. A list of update types the bot is subscribed to. Defaults
    ///   to all update types
    /// </summary>
    [Alias('allowed_updates')]
    property allowed_updates: TArray<String> read Fallowed_updates write Fallowed_updates;
  End;
implementation

uses
  System.SysUtils;

{ TtgApiFileToSend }

constructor TtgFileToSend.Create(const FileName: String; const Content: TStream);
begin
  Self.FFileName := FileName;
  Self.FContent := Content;
end;

destructor TtgFileToSend.Destroy;
begin
  FContent.Free;
  inherited;
end;

{ TtgKeyboardButton }

constructor TtgKeyboardButton.Create(Const Text: String;
  request_contact, request_location: Boolean);
begin
  FText := Text;
  Frequest_contact := request_contact;
  Frequest_location := request_location;
end;

function TtgKeyboardButton.GetFullText: String;
begin
  Result := Text;
end;

{ TtgInlineKeyboardButton }

function TtgInlineKeyboardButton.GetFullText: String;
begin
  Result := Text;
end;

{ TtgChatMember }

destructor TtgChatMember.Destroy;
begin
  if Assigned(Fuser) then FreeAndNil(Fuser);
  inherited;
end;

{ TtgMessageEntity }

destructor TtgMessageEntity.Destroy;
begin
  if Assigned(Fuser) then FreeAndNil(Fuser);
  inherited;
end;

{ TtgDocument }

destructor TtgDocument.Destroy;
begin
  if Assigned(FThumb) then FreeAndNil(FThumb);
  inherited;
end;

{ TtgSticker }

destructor TtgSticker.Destroy;
begin
  if Assigned(FThumb) then FreeAndNil(FThumb);
  inherited;
end;

{ TtgVideo }

destructor TtgVideo.Destroy;
begin
  if Assigned(FThumb) then FreeAndNil(FThumb);
  inherited;
end;

{ TtgVenue }

destructor TtgVenue.Destroy;
begin
  if Assigned(FLocation) then FreeAndNil(FLocation);
  inherited;
end;

{ TtgMessage }

destructor TtgMessage.Destroy;
var
  I: Integer;
begin
  if Assigned(FFrom) then FreeAndNil(FFrom);
  if Assigned(FChat) then FreeAndNil(FChat);
  if Assigned(FForwardFrom) then FreeAndNil(FForwardFrom);
  if Assigned(Fforward_from_chat) then FreeAndNil(Fforward_from_chat);
  if Assigned(FReplyToMessage) then FreeAndNil(FReplyToMessage);
  if Assigned(FAudio) then FreeAndNil(FAudio);
  if Assigned(FDocument) then FreeAndNil(FDocument);
  if Assigned(FSticker) then FreeAndNil(FSticker);
  if Assigned(FVideo) then FreeAndNil(FVideo);
  if Assigned(FVoice) then FreeAndNil(FVoice);
  if Assigned(FContact) then FreeAndNil(FContact);
  if Assigned(FLocation) then FreeAndNil(FLocation);
  if Assigned(FVenue) then FreeAndNil(FVenue);
  if Assigned(FNewChatMember) then FreeAndNil(FNewChatMember);
  if Assigned(FLeftChatMember) then FreeAndNil(FLeftChatMember);
  if Assigned(FPinnedMessage) then FreeAndNil(FPinnedMessage);
  if Assigned(Fforward_from_chat) then FreeAndNil(Fforward_from_chat);
  if Assigned(FNewChatMember) then FreeAndNil(FNewChatMember);
  for I := Low(Fentities) to High(Fentities) do
    FreeAndNil(Fentities[I]);
  SetLength(Fentities, 0);
  for I := Low(FNewChatPhoto) to High(NewChatPhoto) do
    FreeAndNil(FNewChatPhoto[I]);
  SetLength(FNewChatPhoto, 0);
  if Assigned(Fgame) then FreeAndNil(Fgame);
  inherited;
end;

{ TtgUpdate }

destructor TtgUpdate.Destroy;
begin
  if Assigned(FMessage) then FreeAndNil(FMessage);
  if Assigned(FEditedMessage) then FreeAndNil(FEditedMessage);
  if Assigned(Fchannel_post) then FreeAndNil(Fchannel_post);
  if Assigned(Fedited_channel_post) then FreeAndNil(Fedited_channel_post);
  if Assigned(FInlineQuery) then FreeAndNil(FInlineQuery);
  if Assigned(FChosenInlineResult) then FreeAndNil(FChosenInlineResult);
  if Assigned(FCallbackQuery) then FreeAndNil(FCallbackQuery);
  inherited;
end;

{ TtgInlineQueryResult }

destructor TtgInlineQueryResult.Destroy;
begin
  if Assigned(Freply_markup) then FreeAndNil(Freply_markup);
  inherited;
end;

{ TtgAnimation }

destructor TtgAnimation.Destroy;
begin
  if Assigned(Fthumb) then FreeAndNil(Fthumb);
  inherited;
end;

{ TtgGame }

destructor TtgGame.Destroy;
var
  I: Integer;
begin
  for I := Low(Fphoto) to High(Fphoto) do
    if Assigned(Fphoto[i]) then FreeAndNil(Fphoto[i]);
  for I := Low(Ftext_entities) to High(Ftext_entities) do
    if Assigned(Ftext_entities[i]) then FreeAndNil(Ftext_entities[i]);
  if Assigned(Fanimation) then FreeAndNil(Fanimation);
  inherited;
end;

{ TtgGameHighScore }

destructor TtgGameHighScore.Destroy;
begin
  if Assigned(Fuser) then FreeAndNil(Fuser);
  inherited;
end;

end.
