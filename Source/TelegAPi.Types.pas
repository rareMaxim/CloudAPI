unit TelegAPi.Types;

interface

uses
  XSuperObject,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  TelegAPi.Types.Enums;

type

  /// <summary>
  ///   This object represents a Telegram user or bot.
  /// </summary>
  [Alias('User')]
  TtgUser = class
  public
    /// <summary>
    ///   Unique identifier for this user or bot
    /// </summary>
    [Alias('id')]
    ID: Integer;
    /// <summary>
    ///   User‘s or bot’s first name
    /// </summary>
    [Alias('first_name')]
    FirstName: string;
    /// <summary>
    ///   Optional. User‘s or bot’s last name
    /// </summary>
    [Alias('last_name')]
    LastName: string;
    /// <summary>
    ///   Optional. User‘s or bot’s username
    /// </summary>
    [Alias('username')]
    Username: string;
    /// <summary>
    ///   Optional. IETF language tag of the user's language
    /// </summary>
    [Alias('language_code')]
    LanguageCode: string;
  end;

  /// <summary>
  ///   This object contains information about one member of the chat.
  /// </summary>
  [Alias('ChatMember')]
  TtgChatMember = class
  public
    /// <summary>
    ///   Information about the user
    /// </summary>
    [Alias('user')]
    User: TtgUser;
    /// <summary>
    ///   The member's status in the chat. Can be “creator”, “administrator”,
    ///   “member”, “left” or “kicked”
    /// </summary>
    [Alias('status')]
    Status: string;
    /// <summary>
    ///   Optional. Restictred and kicked only. Date when restrictions will be
    ///   lifted for this user, unix time
    /// </summary>
    [Alias('until_date')]
    UntilDate: Integer;
    /// <summary>
    ///   Optional. Administrators only. True, if the bot is allowed to edit
    ///   administrator privileges of that user
    /// </summary>
    [Alias('can_be_edited')]
    CanBeEdited: Boolean;
    /// <summary>
    ///   Optional. Administrators only. True, if the administrator can change
    ///   the chat title, photo and other settings
    /// </summary>
    [Alias('can_change_info')]
    CanChangeInfo: Boolean;
    /// <summary>
    ///   Optional. Administrators only. True, if the administrator can post in
    ///   the channel, channels only
    /// </summary>
    [Alias('can_post_messages')]
    CanPostMessages: Boolean;
    /// <summary>
    ///   Optional. Administrators only. True, if the administrator can edit
    ///   messages of other users, channels only
    /// </summary>
    [Alias('can_edit_messages')]
    CanEditMessages: Boolean;
    /// <summary>
    ///   Optional. Administrators only. True, if the administrator can delete
    ///   messages of other users
    /// </summary>
    [Alias('can_delete_messages')]
    CanDeleteMessages: Boolean;
    /// <summary>
    ///   Optional. Administrators only. True, if the administrator can invite
    ///   new users to the chat
    /// </summary>
    [Alias('can_invite_users')]
    CanInviteUsers: Boolean;
    /// <summary>
    ///   Optional. Administrators only. True, if the administrator can
    ///   restrict, ban or unban chat members
    /// </summary>
    [Alias('can_restrict_members')]
    CanRestrictMembers: Boolean;
    /// <summary>
    ///   Optional. Administrators only. True, if the administrator can pin
    ///   messages, supergroups only
    /// </summary>
    [Alias('can_pin_messages')]
    CanPinMessages: Boolean;
    /// <summary>
    ///   Optional. Administrators only. True, if the administrator can add new
    ///   administrators with a subset of his own privileges or demote
    ///   administrators that he has promoted, directly or indirectly (promoted
    ///   by administrators that were appointed by the user)
    /// </summary>
    [Alias('can_promote_members')]
    CanPromoteMembers: Boolean;
    /// <summary>
    ///   Optional. Restricted only. True, if the user can send text messages,
    ///   contacts, locations and venues
    /// </summary>
    [Alias('can_send_messages')]
    CanSendMessages: Boolean;
    /// <summary>
    ///   Optional. Restricted only. True, if the user can send audios,
    ///   documents, photos, videos, video notes and voice notes, implies <see cref="TelegAPi.Types|TtgChatMember.CanSendMessages">
    ///   CanSendMessages</see>
    /// </summary>
    [Alias('can_send_media_messages')]
    CanSendMediaMessages: Boolean;
    /// <summary>
    ///   Optional. Restricted only. True, if the user can send animations,
    ///   games, stickers and use inline bots, implies <see cref="TelegAPi.Types|TtgChatMember.CanSendMediaMessages">
    ///   CanSendMediaMessages</see>
    /// </summary>
    [Alias('can_send_other_messages')]
    CanSendOtherMessages: Boolean;
    /// <summary>
    ///   Optional. Restricted only. True, if user may add web page previews to
    ///   his messages, implies <see cref="TelegAPi.Types|TtgChatMember.CanSendMediaMessages">
    ///   CanSendMediaMessages</see>
    /// </summary>
    [Alias('can_add_web_page_previews')]
    CanAddWebPagePreviews: Boolean;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   This object represents a chat.
  /// </summary>
  [Alias('Chat')]
  TtgChat = class
  public
    /// <summary>
    ///   Unique identifier for this chat, not exceeding 1e13 by absolute value
    /// </summary>
    [Alias('id')]
    ID: Int64;
    /// <summary>
    ///   Type of chat, can be either “private”, “group”, “supergroup” or
    ///   “channel”
    /// </summary>
    [Alias('type')]
    TypeChat: string;
    /// <summary>
    ///   Optional. Title, for channels and group chats
    /// </summary>
    [Alias('title')]
    Title: string;
    /// <summary>
    ///   Optional. Username, for private chats and channels if available
    /// </summary>
    [Alias('username')]
    Username: string;
    /// <summary>
    ///   Optional. First name of the other party in a private chat
    /// </summary>
    [Alias('first_name')]
    FirstName: string;
    /// <summary>
    ///   Optional. Last name of the other party in a private chat
    /// </summary>
    [Alias('last_name')]
    LastName: string;
    /// <summary>
    ///   Optional. True if a group has ‘All Members Are Admins’ enabled.
    /// </summary>
    [Alias('all_members_are_administrators')]
    AllMembersAreAdministrators: Boolean;
  end;

  /// <summary>
  ///   This object represents one special entity in a text message. For
  ///   example, hashtags, usernames, URLs, etc.
  /// </summary>
  [Alias('MessageEntity')]
  TtgMessageEntity = class
  public
    /// <summary>
    ///   Type of the entity. One of mention (@username), hashtag, bot_command,
    ///   url, email, bold (bold text), italic (italic text), code (monowidth
    ///   string), pre (monowidth block), text_link (for clickable text URLs),
    ///   text_mention (for users without usernames)
    /// </summary>
    [Alias('type')]
    TypeMessage: string;
    /// <summary>
    ///   Offset in UTF-16 code units to the start of the entity
    /// </summary>
    [Alias('offset')]
    Offset: Integer;
    /// <summary>
    ///   Length of the entity in UTF-16 code units
    /// </summary>
    [Alias('length')]
    Length: Integer;
    /// <summary>
    ///   Optional. For “text_link” only, url that will be opened after user
    ///   taps on the text
    /// </summary>
    [Alias('url')]
    Url: string;
    /// <summary>
    ///   Optional. For “text_mention” only, the mentioned user
    /// </summary>
    [Alias('user')]
    User: TtgUser;
    destructor Destroy; override;
  end;

  [Alias('File')]
  TtgFile = class
  public
    [Alias('file_id')]
    FileId: string;
    [Alias('file_size')]
    FileSize: Integer;
    [Alias('file_path')]
    FilePath: string;
    function GetFileUrl(const AToken: string): string;
  end;

  /// <summary>
  ///   This object represents an audio file to be treated as music by the
  ///   Telegram clients.
  /// </summary>
  [Alias('Audio')]
  TtgAudio = class(TtgFile)
  public
    /// <summary>
    ///   Duration of the audio in seconds as defined by sender
    /// </summary>
    [Alias('duration')]
    Duration: Integer;
    /// <summary>
    ///   Performer of the audio as defined by sender or by audio tags
    /// </summary>
    [Alias('performer')]
    Performer: string;
    /// <summary>
    ///   Title of the audio as defined by sender or by audio tags
    /// </summary>
    [Alias('title')]
    Title: string;
    /// <summary>
    ///   Optional. MIME type of the file as defined by sender
    /// </summary>
    [Alias('mime_type')]
    MimeType: string;
  end;

  /// <summary>
  ///   This object represents one size of a photo or a file/sticker thumbnail.
  /// </summary>
  /// <remarks>
  ///   A missing thumbnail for a file (or sticker) is presented as an empty
  ///   object.
  /// </remarks>
  [Alias('PhotoSize')]
  TtgPhotoSize = class(TtgFile)
  public
    /// <summary>
    ///   Photo width
    /// </summary>
    [Alias('width')]
    Width: Integer;
    /// <summary>
    ///   Photo height
    /// </summary>
    [Alias('Height')]
    Height: Integer;
  end;

  /// <summary>
  ///   This object represents a general file (as opposed to photos, voice
  ///   messages and audio files).
  /// </summary>
  [Alias('Document')]
  TtgDocument = class(TtgFile)
  public
    /// <summary>
    ///   Document thumbnail as defined by sender
    /// </summary>
    [Alias('thumb')]
    Thumb: TtgPhotoSize;
    /// <summary>
    ///   Optional. Original filename as defined by sender
    /// </summary>
    [Alias('file_name')]
    FileName: string;
    /// <summary>
    ///   Optional. MIME type of the file as defined by sender
    /// </summary>
    [Alias('mime_type')]
    MimeType: string;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   This object represents a sticker.
  /// </summary>
  [Alias('Sticker')]
  TtgSticker = class(TtgFile)
  public
    /// <summary>
    ///   Sticker width
    /// </summary>
    [Alias('width')]
    Width: Integer;
    /// <summary>
    ///   Sticker height
    /// </summary>
    [Alias('width')]
    Height: Integer;
    /// <summary>
    ///   Sticker thumbnail in .webp or .jpg format
    /// </summary>
    [Alias('thumb')]
    Thumb: TtgPhotoSize;
    /// <summary>
    ///   Optional. Emoji associated with the sticker
    /// </summary>
    [Alias('emoji')]
    Emoji: string;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   This object represents a video file.
  /// </summary>
  [Alias('Video')]
  TtgVideo = class(TtgFile)
  public
    /// <summary>
    ///   Video width as defined by sender
    /// </summary>
    [Alias('width')]
    Width: Integer;
    /// <summary>
    ///   Video height as defined by sender
    /// </summary>
    [Alias('height')]
    Height: Integer;
    /// <summary>
    ///   Duration of the video in seconds as defined by sender
    /// </summary>
    [Alias('duration')]
    Duration: Integer;
    /// <summary>
    ///   Video thumbnail
    /// </summary>
    [Alias('thumb')]
    Thumb: TtgPhotoSize;
    /// <summary>
    ///   Optional. Mime type of a file as defined by sender
    /// </summary>
    [Alias('mime_type')]
    MimeType: string;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   This object represents a video message
  /// </summary>
  /// <remarks>
  ///   available in Telegram apps as of v.4.0
  /// </remarks>
  [Alias('VideoNote')]
  TtgVideoNote = class
  public
    /// <summary>
    ///   Unique identifier for this file
    /// </summary>
    [Alias('file_id')]
    FileId: string;
    /// <summary>
    ///   Video width and height as defined by sender
    /// </summary>
    [Alias('length')]
    Length: Integer;
    /// <summary>
    ///   Duration of the video in seconds as defined by sender
    /// </summary>
    [Alias('duration')]
    Duration: Integer;
    /// <summary>
    ///   Optional. Video thumbnail
    /// </summary>
    [Alias('thumb')]
    Thumb: TtgPhotoSize;
    /// <summary>
    ///   Optional. File size
    /// </summary>
    [Alias('file_size')]
    FileSize: Integer;
  end;

  /// <summary>
  ///   This object represents a voice note.
  /// </summary>
  [Alias('Voice')]
  TtgVoice = class(TtgFile)
  public
    /// <summary>
    ///   Duration of the audio in seconds as defined by sender
    /// </summary>
    [Alias('duration')]
    Duration: Integer;
    /// <summary>
    ///   Optional. MIME type of the file as defined by sender
    /// </summary>
    [Alias('mime_type')]
    MimeType: string;
  end;

  /// <summary>
  ///   This object represents a phone contact.
  /// </summary>
  [Alias('Contact')]
  TtgContact = class
  public
    /// <summary>
    ///   Contact's phone number
    /// </summary>
    [Alias('phone_number')]
    PhoneNumber: string;
    /// <summary>
    ///   Contact's first name
    /// </summary>
    [Alias('first_name')]
    FirstName: string;
    /// <summary>
    ///   Optional. Contact's last name
    /// </summary>
    [Alias('last_name')]
    LastName: string;
    /// <summary>
    ///   Optional. Contact's user identifier in Telegram
    /// </summary>
    [Alias('user_id')]
    UserId: Integer;
  end;

  /// <summary>
  ///   This object represents a point on the map.
  /// </summary>
  [Alias('Location')]
  TtgLocation = class
  public
    /// <summary>
    ///   Longitude as defined by sender
    /// </summary>
    [Alias('longitude')]
    Longitude: Single;
    /// <summary>
    ///   Latitude as defined by sender
    /// </summary>
    [Alias('latitude')]
    Latitude: Single;
    constructor Create(ALongitude, ALatitude: Single);
  end;

  /// <summary>
  ///   This object represents a venue.
  /// </summary>
  [Alias('Venue')]
  TtgVenue = class
  public
    /// <summary>
    ///   Venue location
    /// </summary>
    [Alias('location')]
    Location: TtgLocation;
    /// <summary>
    ///   Title of the result
    /// </summary>
    [Alias('title')]
    Title: string;
    /// <summary>
    ///   Address of the venue
    /// </summary>
    [Alias('address')]
    Address: string;
    /// <summary>
    ///   Optional. Foursquare identifier of the venue
    /// </summary>
    [Alias('foursquare_id')]
    FoursquareId: string;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   You can provide an animation for your game so that it looks stylish in
  ///   chats (check out Lumberjack for an example). This object represents an
  ///   animation file to be displayed in the message containing a game.
  /// </summary>
  [Alias('Animation')]
  TtgAnimation = class
  public
    /// <summary>
    ///   Unique file identifier
    /// </summary>
    [Alias('file_id')]
    FileId: string;
    /// <summary>
    ///   Optional. Animation thumbnail as defined by sender
    /// </summary>
    [Alias('thumb')]
    Thumb: TtgPhotoSize;
    /// <summary>
    ///   Optional. Original animation filename as defined by sender
    /// </summary>
    [Alias('file_name')]
    FileName: string;
    /// <summary>
    ///   Optional. MIME type of the file as defined by sender
    /// </summary>
    [Alias('mime_type')]
    MimeType: string;
    /// <summary>
    ///   Optional. File size
    /// </summary>
    [Alias('file_size')]
    FileSize: Integer;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   This object represents one row of the high scores table for a game.
  /// </summary>
  [Alias('Game')]
  TtgGameHighScore = class
  public
    /// <summary>
    ///   Position in high score table for the game
    /// </summary>
    [Alias('position')]
    Position: Integer;
    /// <summary>
    ///   User
    /// </summary>
    [Alias('user')]
    User: TtgUser;
    /// <summary>
    ///   Score
    /// </summary>
    [Alias('score')]
    Score: Integer;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   This object represents a game. Use BotFather to create and edit games,
  ///   their short names will act as unique identifiers.
  /// </summary>
  [Alias('Game')]
  TtgGame = class
  public
    /// <summary>
    ///   Title of the game
    /// </summary>
    [Alias('title')]
    Title: string;
    /// <summary>
    ///   Description of the game
    /// </summary>
    [Alias('description')]
    Description: string;
    /// <summary>
    ///   Photo that will be displayed in the game message in chats.
    /// </summary>
    [Alias('photo')]
    Photo: TObjectList<TtgPhotoSize>;
    /// <summary>
    ///   Optional. Brief description of the game or high scores included in
    ///   the game message. Can be automatically edited to include current high
    ///   scores for the game when the bot calls setGameScore, or manually
    ///   edited using editMessageText. 0-4096 characters.
    /// </summary>
    [Alias('text')]
    Text: string;
    /// <summary>
    ///   Optional. Special entities that appear in text, such as usernames,
    ///   URLs, bot commands, etc.
    /// </summary>
    [Alias('text_entities')]
    TextEntities: TObjectList<TtgMessageEntity>;
    /// <summary>
    ///   Optional. Animation that will be displayed in the game message in
    ///   chats. Upload via BotFather
    /// </summary>
    [Alias('animation')]
    Animation: TtgAnimation;
    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   This object represents a message.
  /// </summary>
  [Alias('Message')]
  TtgMessage = class
  public
    /// <summary>
    ///   Unique message identifier
    /// </summary>
    [Alias('message_id')]
    MessageId: Integer;
    /// <summary>
    ///   Sender
    /// </summary>
    [Alias('from')]
    From: TtgUser;
    /// <summary>
    ///   Date the message was sent in Unix time
    /// </summary>
    [Alias('date')]
    Date: Integer;
    /// <summary>
    ///   Conversation the message belongs to
    /// </summary>
    [Alias('chat')]
    Chat: TtgChat;
    /// <summary>
    ///   Optional. For forwarded messages, sender of the original message
    /// </summary>
    [Alias('forward_from')]
    ForwardFrom: TtgUser;
    /// <summary>
    ///   Optional. For messages forwarded from a channel, information about
    ///   the original channel
    /// </summary>
    [Alias('forward_from_chat')]
    ForwardFromChat: TtgChat;
    /// <summary>
    ///   Optional. For forwarded messages, date the original message was sent
    ///   in Unix time
    /// </summary>
    [Alias('forward_date')]
    ForwardDate: Integer;
    /// <summary>
    ///   Optional. For replies, the original message. Note that the Message
    ///   object in this field will not contain further reply_to_message fields
    ///   even if it itself is a reply.
    /// </summary>
    [Alias('reply_to_message')]
    ReplyToMessage: TtgMessage;
    /// <summary>
    ///   Optional. Date the message was last edited in Unix time.
    /// </summary>
    [Alias('edit_date')]
    EditDate: Integer;
    /// <summary>
    ///   Optional. For text messages, the actual UTF-8 text of the message
    /// </summary>
    [Alias('text')]
    Text: string;
    /// <summary>
    ///   Optional. For text messages, special entities like usernames, URLs,
    ///   bot commands, etc. that appear in the text
    /// </summary>
    [Alias('entities')]
    Entities: TObjectList<TtgMessageEntity>;
    /// <summary>
    ///   Optional. Message is an audio file, information about the file
    /// </summary>
    [Alias('audio')]
    Audio: TtgAudio;
    /// <summary>
    ///   Optional. Message is a general file, information about the file
    /// </summary>
    [Alias('document')]
    Document: TtgDocument;
    /// <summary>
    ///   Optional. Message is a game, information about the game.
    /// </summary>
    [Alias('game')]
    Game: TtgGame;
    /// <summary>
    ///   Optional. Message is a photo, available sizes of the photo
    /// </summary>
    [Alias('photo')]
    Photo: TObjectList<TtgPhotoSize>;
    /// <summary>
    ///   Optional. Message is a sticker, information about the sticker
    /// </summary>
    [Alias('sticker')]
    Sticker: TtgSticker;
    /// <summary>
    ///   Optional. Message is a video, information about the video
    /// </summary>
    [Alias('video')]
    Video: TtgVideo;
    /// <summary>
    ///   Message is a voice message, information about the file
    /// </summary>
    [Alias('voice')]
    Voice: TtgVoice;
    /// <summary>
    ///   Optional. Message is a video note, information about the video
    ///   message
    /// </summary>
    [Alias('video_note')]
    VideoNote: TtgVideoNote;
    /// <summary>
    ///   Optional. New members that were added to the group or supergroup and
    ///   information about them (the bot itself may be one of these members)
    /// </summary>
    [Alias('new_chat_members')]
    NewChatMembers: TObjectList<TtgUser>;
    /// <summary>
    ///   Optional. Caption for the document, photo or video, 0-200 characters
    /// </summary>
    [Alias('caption')]
    Caption: string;
    /// <summary>
    ///   Optional. Message is a shared contact, information about the contact
    /// </summary>
    [Alias('contact')]
    Contact: TtgContact;
    /// <summary>
    ///   Optional. Message is a shared location, information about the
    ///   location
    /// </summary>
    [Alias('location')]
    Location: TtgLocation;
    /// <summary>
    ///   Optional. Message is a venue, information about the venue
    /// </summary>
    [Alias('venue')]
    Venue: TtgVenue;
    /// <summary>
    ///   Optional. A new member was added to the group, information about them
    ///   (this member may be the bot itself)
    /// </summary>
    [Alias('new_chat_member')]
    NewChatMember: TtgUser;
    /// <summary>
    ///   Optional. A member was removed from the group, information about them
    ///   (this member may be bot itself)
    /// </summary>
    [Alias('left_chat_member')]
    LeftChatMember: TtgUser;
    /// <summary>
    ///   Optional. A group title was changed to this value
    /// </summary>
    [Alias('new_chat_title')]
    NewChatTitle: string;
    /// <summary>
    ///   Optional. A group photo was change to this value
    /// </summary>
    [Alias('new_chat_photo')]
    NewChatPhoto: TObjectList<TtgPhotoSize>;
    /// <summary>
    ///   Optional. Informs that the group photo was deleted
    /// </summary>
    [Alias('delete_chat_photo')]
    DeleteChatPhoto: Boolean;
    /// <summary>
    ///   Optional. Informs that the group has been created
    /// </summary>
    [Alias('group_chat_created')]
    GroupChatCreated: Boolean;
    /// <summary>
    ///   Optional. Service message: the supergroup has been created
    /// </summary>
    [Alias('supergroup_chat_created')]
    SupergroupChatCreated: Boolean;
    /// <summary>
    ///   Optional. Service message: the channel has been created
    /// </summary>
    [Alias('channel_chat_created')]
    ChannelChatCreated: Boolean;
    /// <summary>
    ///   Optional. The group has been migrated to a supergroup with the
    ///   specified identifier
    /// </summary>
    [Alias('migrate_to_chat_id')]
    MigrateToChatId: Int64;
    /// <summary>
    ///   Optional. The supergroup has been migrated from a group with the
    ///   specified identifier
    /// </summary>
    [Alias('migrate_from_chat_id')]
    MigrateFromChatId: Int64;
    /// <summary>
    ///   Optional. Specified message was pinned. Note that the Message object
    ///   in this field will not contain further reply_to_message fields even
    ///   if it is itself a reply
    /// </summary>
    [Alias('pinned_message')]
    PinnedMessage: TtgMessage;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   This object represent a user's profile pictures.
  /// </summary>
  [Alias('UserProfilePhotos')]
  TtgUserProfilePhotos = class
  public
    /// <summary>
    ///   Total number of profile pictures the target user has
    /// </summary>
    [Alias('total_count')]
    TotalCount: Integer;
    /// <summary>
    ///   Requested profile pictures (in up to 4 sizes each)
    /// </summary>
    [Alias('photos')]
    Photos: TObjectList<TObjectList<TtgPhotoSize>>;
    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   This object represents one button of the reply keyboard. For simple
  ///   text buttons String can be used instead of this object to specify text
  ///   of the button. Optional fields are mutually exclusive.
  /// </summary>
  /// <remarks>
  ///   request_contact and request_location options will only work in Telegram
  ///   versions released after 9 April, 2016. Older clients will ignore them.
  /// </remarks>
  [Alias('KeyboardButton')]
  TtgKeyboardButton = class(TObject)
  public
    /// <summary>
    ///   Text of the button. If none of the optional fields are used, it will
    ///   be sent to the bot as a message when the button is pressed
    /// </summary>
    [Alias('text')]
    Text: string;
    /// <summary>
    ///   Optional. If True, the user's phone number will be sent as a contact
    ///   when the button is pressed. Available in private chats only
    /// </summary>
    [Alias('request_contact')]
    RequestContact: Boolean;
    /// <summary>
    ///   Optional. If True, the user's current location will be sent when the
    ///   button is pressed. Available in private chats only
    /// </summary>
    [Alias('request_location')]
    RequestLocation: Boolean;
    constructor Create(const AText: string; ARequestContact: Boolean = False; ARequestLocation: Boolean = False); overload;
  end;

  /// <summary>
  ///   A placeholder, currently holds no information.
  /// </summary>

  TtgCallbackGame = class
  end;

  /// <summary>
  ///   This object represents one button of an inline keyboard. You must use
  ///   exactly one of the optional fields.
  /// </summary>
  TtgInlineKeyboardButton = class
  public
    /// <summary>
    ///   Label text on the button
    /// </summary>
    [Alias('text')]
    Text: string;
    /// <summary>
    ///   Optional. HTTP url to be opened when button is pressed
    /// </summary>
    [Alias('url')]
    Url: string;
    /// <summary>
    ///   Optional. Data to be sent in a callback query to the bot when button
    ///   is pressed, 1-64 bytes
    /// </summary>
    [Alias('callback_data')]
    CallbackData: string;
    /// <summary>
    ///   Optional. If set, pressing the button will prompt the user to select
    ///   one of their chats, open that chat and insert the bot‘s username and
    ///   the specified inline query in the input field. Can be empty, in which
    ///   case just the bot’s username will be inserted.
    /// </summary>
    /// <remarks>
    ///   Note: This offers an easy way for users to start using your bot in
    ///   inline mode when they are currently in a private chat with it.
    ///   Especially useful when combined with switch_pm… actions – in this
    ///   case the user will be automatically returned to the chat they
    ///   switched from, skipping the chat selection screen.
    /// </remarks>
    [Alias('switch_inline_query')]
    SwitchInlineQuery: string;
    /// <summary>
    ///   Optional. If set, pressing the button will insert the bot‘s username
    ///   and the specified inline query in the current chat's input field. Can
    ///   be empty, in which case only the bot’s username will be inserted. <br /><br />
    ///    This offers a quick way for the user to open your bot in inline mode
    ///   in the same chat – good for selecting something from multiple
    ///   options.
    /// </summary>
    [Alias('switch_inline_query_current_chat')]
    SwitchInlineQueryCurrentChat: string;
    /// <summary>
    ///   Optional. Description of the game that will be launched when the user
    ///   presses the button. <br /><br />
    /// </summary>
    /// <remarks>
    ///   NOTE: This type of button must always be the first button in the
    ///   first row.
    /// </remarks>
    [Alias('callback_game')]
    CallbackGame: TtgCallbackGame;
    /// <summary>
    ///   Optional. Specify True, to send a Pay button. <br /><br />
    /// </summary>
    /// <remarks>
    ///   NOTE: This type of button must always be the first button in the
    ///   first row.
    /// </remarks>
    [Alias('pay')]
    Pay: Boolean;
    /// <summary>
    ///   Initializes a new instance of the <see cref="TelegAPi.Types|TtgKeyboardButton" />
    ///    class.
    /// </summary>
    /// <param name="AText">
    ///   Text of the button
    /// </param>
    constructor Create(const AText: string); overload;
    /// <summary>
    ///   Initializes a new instance of the <see cref="TelegAPi.Types|TtgKeyboardButton" />
    ///    class.
    /// </summary>
    /// <param name="AText">
    ///   The text. <br />
    /// </param>
    /// <param name="ACallbackData">
    ///   The callback data. <br />
    /// </param>
    constructor Create(const AText, ACallbackData: string); overload;
    /// <summary>
    ///   Initializes a new instance of the <see cref="TelegAPi.Types|TtgKeyboardButton" />
    ///    class.
    /// </summary>
    /// <param name="AText">
    ///   Text of the button <br />
    /// </param>
    constructor Create(const AText: string; ACallbackGame: TtgCallbackGame); overload;
  end;

  /// <summary>
  ///   Contains information about why a request was unsuccessfull.
  /// </summary>
  TrgResponseParameters = class
  public
    /// <summary>
    ///   The group has been migrated to a supergroup with the specified
    ///   identifier.
    /// </summary>
    [Alias('migrate_to_chat_id')]
    MigrateToChatId: Int64;
    /// <summary>
    ///   In case of exceeding flood control, the number of seconds left to
    ///   wait before the request can be repeated.
    /// </summary>
    [Alias('retry_after')]
    RetryAfter: Integer;
  end;

  [Alias('')]
  TtgApiResponse<T> = class
  public
    /// <summary>
    ///   Gets a value indicating whether the request was successful.
    /// </summary>
    [Alias('ok')]
    Ok: Boolean;
    /// <summary>
    ///   Gets the result object.
    /// </summary>
    /// <value>
    ///   The result object.
    /// </value>
    [Alias('result')]
    ResultObject: T;
    /// <summary>
    ///   Gets the error message.
    /// </summary>
    /// <value>
    ///   The error message.
    /// </value>
    [Alias('description')]
    message: string;
    /// <summary>
    ///   Gets the error code.
    /// </summary>
    /// <value>
    ///   The error code
    /// </value>
    [Alias('error_code')]
    Code: Integer;
    /// <summary>
    ///   Contains information about why a request was unsuccessfull.
    /// </summary>
    [Alias('parameters')]
    Parameters: TrgResponseParameters;
    destructor Destroy; override;
  end;

  [Alias('FileToSend')]
  TtgFileToSend = class
  private
    procedure DoCheckParams;
  public
    FileName: string;
    Content: TStream;
    constructor Create(const FileName: string; const Content: TStream);
    destructor Destroy; override;
  end;

  /// <summary>
  ///   This object represents an incoming inline query. When the user sends an
  ///   empty query, your bot could return some default or trending results.
  /// </summary>
  [Alias('InlineQuery')]
  TtgInlineQuery = class
  public
    /// <summary>
    ///   Unique identifier for this query
    /// </summary>
    [Alias('id')]
    ID: string;
    /// <summary>
    ///   Sender
    /// </summary>
    [Alias('from')]
    From: TtgUser;
    /// <summary>
    ///   Text of the query
    /// </summary>
    [Alias('query')]
    Query: string;
    /// <summary>
    ///   Offset of the results to be returned, can be controlled by the bot
    /// </summary>
    [Alias('offset')]
    Offset: string;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   Represents a result of an inline query that was chosen by the user and
  ///   sent to their chat partner.
  /// </summary>
  [Alias('ChosenInlineResult')]
  TtgChosenInlineResult = class
  public
    /// <summary>
    ///   The unique identifier for the result that was chosen.
    /// </summary>
    [Alias('result_id')]
    ResultId: string;
    /// <summary>
    ///   The user that chose the result.
    /// </summary>
    [Alias('from')]
    From: TtgUser;
    /// <summary>
    ///   The query that was used to obtain the result.
    /// </summary>
    [Alias('location')]
    Location: TtgLocation;
    /// <summary>
    ///   Optional. Identifier of the sent inline message. Available only if
    ///   there is an inline keyboard attached to the message. Will be also
    ///   received in callback queries and can be used to edit the message.
    /// </summary>
    [Alias('inline_message_id')]
    InlineMessageId: string;
    /// <summary>
    ///   The query that was used to obtain the result.
    /// </summary>
    [Alias('query')]
    Query: string;
  end;

  [Alias('CallbackQuery')]
  TtgCallbackQuery = class
  public
    /// <summary>
    ///   Unique identifier for this query
    /// </summary>
    [Alias('id')]
    ID: string;
    /// <summary>
    ///   Sender
    /// </summary>
    [Alias('from')]
    From: TtgUser;
    /// <summary>
    ///   Optional. Message with the callback button that originated the query.
    ///   Note that message content and message date will not be available if
    ///   the message is too old
    /// </summary>
    [Alias('message')]
    message: TtgMessage;
    /// <summary>
    ///   Optional. Identifier of the message sent via the bot in inline mode,
    ///   that originated the query
    /// </summary>
    [Alias('inline_message_id')]
    InlineMessageId: string;
    /// <summary>
    ///   Data associated with the callback button. Be aware that a bad client
    ///   can send arbitrary data in this field
    /// </summary>
    [Alias('data')]
    Data: string;
    /// <summary>
    ///   Optional. Short name of a Game to be returned, serves as the unique
    /// </summary>
    [Alias('game_short_name')]
    GameShortName: string;
    destructor Destroy; override;
  end;

{$REGION 'Payments'}

  /// <summary>
  ///   This object contains basic information about an invoice.
  /// </summary>
  /// <seealso href="https://core.telegram.org/bots/api#invoice" />
  TtgInvoice = class
  public
    /// <summary>
    ///   Product name
    /// </summary>
    [Alias('title')]
    Title: string;
    /// <summary>
    ///   Product description
    /// </summary>
    [Alias('description')]
    Description: string;
    /// <summary>
    ///   Unique bot deep-linking parameter that can be used to generate this
    ///   invoice
    /// </summary>
    [Alias('start_parameter')]
    StartParameter: string;
    /// <summary>
    ///   Three-letter ISO 4217 <see href="https://core.telegram.org/bots/payments#supported-currencies">
    ///   currency</see> code
    /// </summary>
    [Alias('currency')]
    Currency: string;
    /// <summary>
    ///   Total price in the smallest units of the currency (integer, not
    ///   float/double). For example, for a price of <c>US$ 1.45</c> pass <c>
    ///   amount = 145</c>. See the <c>exp</c> parameter in <see href="https://core.telegram.org/bots/payments/currencies.json">
    ///   currencies.json</see>, it shows the number of digits past the decimal
    ///   point for each currency (2 for the majority of currencies).
    /// </summary>
    [Alias('total_amount')]
    TotalAmount: Integer;
  end;

  /// <summary>
  ///   This object represents a portion of the price for goods or services.
  /// </summary>
  [Alias('LabeledPrice')]
  TtgLabeledPrice = class
  public
    /// <summary>
    ///   Portion label
    /// </summary>
    [Alias('label')]
    Text: string;
    /// <summary>
    ///   Price of the product in the smallest units of the <see href="https://core.telegram.org/bots/payments#supported-currencies">
    ///   currency</see> (integer, not float/double).
    /// </summary>
    /// <example>
    ///   For example, for a price of <c>US$ 1.45</c> pass <c>amount = 145</c>
    ///   . See the exp parameter in <see href="https://core.telegram.org/bots/payments/currencies.json">
    ///   currencies.json</see>, it shows the number of digits past the decimal
    ///   point for each <br />currency (2 for the majority of currencies). <br />
    /// </example>
    Amount: Integer;
  end;

  /// <summary>
  ///   This object represents a shipping address.
  /// </summary>
  TtgShippingAddress = class
  public
    /// <summary>
    ///   ISO 3166-1 alpha-2 country code
    /// </summary>
    [Alias('country_code')]
    CountryCode: string;
    /// <summary>
    ///   State, if applicable
    /// </summary>
    [Alias('state')]
    State: string;
    /// <summary>
    ///   City
    /// </summary>
    [Alias('city')]
    City: string;
    /// <summary>
    ///   First line for the address
    /// </summary>
    [Alias('street_line1')]
    StreetLine1: string;
    /// <summary>
    ///   Second line for the address
    /// </summary>
    [Alias('street_line2')]
    StreetLine2: string;
    /// <summary>
    ///   Address post code
    /// </summary>
    [Alias('post_code')]
    PostCode: string;
  end;

  /// <summary>
  ///   This object represents information about an order.
  /// </summary>
  TtgOrderInfo = class
  public
    /// <summary>
    ///   Optional. User name
    /// </summary>
    [Alias('name')]
    Name: string;
    /// <summary>
    ///   Optional. User's phone number
    /// </summary>
    [Alias('phone_number')]
    PhoneNumber: string;
    /// <summary>
    ///   Optional. User email
    /// </summary>
    [Alias('email')]
    Email: string;
    /// <summary>
    ///   Optional. User shipping address
    /// </summary>
    [Alias('shipping_address')]
    ShippingAddress: TtgShippingAddress;
  end;

  /// <summary>
  ///   This object contains information about an incoming pre-checkout query.
  /// </summary>
  [Alias('PreCheckoutQuery')]
  TtgPreCheckoutQuery = class
  public
    /// <summary>
    ///   Unique query identifier
    /// </summary>
    [Alias('id')]
    ID: string;
    /// <summary>
    ///   User who sent the query
    /// </summary>
    [Alias('from')]
    From: TtgUser;
    /// <summary>
    ///   Three-letter ISO 4217 <see href="https://core.telegram.org/bots/payments#supported-currencies">
    ///   currency</see> code
    /// </summary>
    [Alias('currency')]
    Currency: string;
    /// <summary>
    ///   Total price in the smallest units of the currency (integer, not
    ///   float/double). For example, for a price of <c>US$ 1.45</c> pass <c>
    ///   amount = 145</c>. See the <c>exp</c> parameter in <see href="https://core.telegram.org/bots/payments/currencies.json">
    ///   currencies.json</see>, it shows the number of digits past the decimal
    ///   point for each currency (2 for the majority of currencies).
    /// </summary>
    [Alias('total_amount')]
    TotalAmount: Integer;
    /// <summary>
    ///   Bot specified invoice payload
    /// </summary>
    [Alias('invoice_payload')]
    InvoicePayload: string;
    /// <summary>
    ///   Optional. Identifier of the shipping option chosen by the user
    /// </summary>
    [Alias('shipping_option_id')]
    ShippingOptionId: string;
    /// <summary>
    ///   Optional. Order info provided by the user
    /// </summary>
    [Alias('order_info')]
    OrderInfo: TtgOrderInfo;
  end;

  /// <summary>
  ///   This object represents one shipping option.
  /// </summary>
  TtgShippingOption = class
  public
    /// <summary>
    ///   Shipping option identifier
    /// </summary>
    [Alias('id')]
    ID: string;
    /// <summary>
    ///   Option title
    /// </summary>
    [Alias('title')]
    Title: string;
    /// <summary>
    ///   List of price portions
    /// </summary>
    [Alias('prices')]
    Prices: TObjectList<TtgLabeledPrice>;
    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   This object contains information about an incoming shipping query.
  /// </summary>
  TtgShippingQuery = class
  public
    /// <summary>
    ///   Unique query identifier
    /// </summary>
    [Alias('id')]
    ID: string;
    /// <summary>
    ///   User who sent the query
    /// </summary>
    [Alias('from')]
    From: TtgUser;
    /// <summary>
    ///   Bot specified invoice payload
    /// </summary>
    [Alias('invoice_payload')]
    InvoicePayload: string;
    /// <summary>
    ///   User specified shipping address
    /// </summary>
    [Alias('shipping_address')]
    ShippingAddress: TtgShippingAddress;
  end;

  /// <summary>
  ///   This object contains basic information about a successful payment.
  /// </summary>
  TtgSuccessfulPayment = class
  public
    /// <summary>
    ///   Three-letter ISO 4217 <see href="https://core.telegram.org/bots/payments#supported-currencies">
    ///   currency</see> code
    /// </summary>
    [Alias('currency')]
    Currency: string;
    /// <summary>
    ///   Total price in the smallest units of the currency (integer, not
    ///   float/double). For example, for a price of <c>US$ 1.45</c> pass <c>
    ///   amount = 145</c>. See the <c>exp</c> parameter in <see href="https://core.telegram.org/bots/payments/currencies.json">
    ///   currencies.json</see>, it shows the number of digits past the decimal
    ///   point for each currency (2 for the majority of currencies).
    /// </summary>
    [Alias('total_amount')]
    TotalAmount: Integer;
    /// <summary>
    ///   Bot specified invoice payload
    /// </summary>
    [Alias('invoice_payload')]
    InvoicePayload: string;
    /// <summary>
    ///   Optional. Identifier of the shipping option chosen by the user
    /// </summary>
    [Alias('shipping_option_id')]
    ShippingOptionId: string;
    /// <summary>
    ///   Optional. Order info provided by the user
    /// </summary>
    [Alias('order_info')]
    OrderInfo: TtgOrderInfo;
    /// <summary>
    ///   Telegram payment identifier
    /// </summary>
    [Alias('telegram_payment_charge_id')]
    TelegramPaymentChargeId: string;
    /// <summary>
    ///   Provider payment identifier
    /// </summary>
    [Alias('provider_payment_charge_id')]
    ProviderPaymentChargeId: string;
  end;
{$ENDREGION}

  /// <summary>
  ///   This object represents an incoming update.
  /// </summary>
  /// <remarks>
  ///   Only one of the optional parameters can be present in any given update.
  /// </remarks>

  [Alias('Update')]
  TtgUpdate = class
  public
    /// <summary>
    ///   The update‘s unique identifier. Update identifiers start from a
    ///   certain positive number and increase sequentially. This ID becomes
    ///   especially handy if you’re using Webhooks, since it allows you to
    ///   ignore repeated updates or to restore the correct update sequence,
    ///   should they get out of order.
    /// </summary>
    [Alias('update_id')]
    ID: Integer;
    /// <summary>
    ///   Optional. New incoming message of any kind — text, photo, sticker,
    ///   etc.
    /// </summary>
    [Alias('message')]
    message: TtgMessage;
    /// <summary>
    ///   Optional. New version of a message that is known to the bot and was
    ///   edited
    /// </summary>
    [Alias('edited_message')]
    EditedMessage: TtgMessage;
    /// <summary>
    ///   Optional. New incoming inline query
    /// </summary>
    [Alias('inline_query')]
    InlineQuery: TtgInlineQuery;
    /// <summary>
    ///   Optional. The result of a inline query that was chosen by a user and
    ///   sent to their chat partner
    /// </summary>
    [Alias('chosen_inline_result')]
    ChosenInlineResult: TtgChosenInlineResult;
    /// <summary>
    ///   Optional. New incoming callback query
    /// </summary>
    [Alias('callback_query')]
    CallbackQuery: TtgCallbackQuery;
    /// <summary>
    ///   Optional. New incoming channel post of any kind — text, photo,
    ///   sticker, etc.
    /// </summary>
    [Alias('channel_post')]
    ChannelPost: TtgMessage;
    /// <summary>
    ///   Optional. New version of a channel post that is known to the bot and
    ///   was edited
    /// </summary>
    [Alias('edited_channel_post')]
    EditedChannelPost: TtgMessage;
    /// <summary>
    ///   Optional. New incoming shipping query. Only for invoices with
    ///   flexible price
    /// </summary>
    [Alias('shipping_query')]
    ShippingQuery: TtgShippingQuery;
    /// <summary>
    ///   Optional. New incoming pre-checkout query. Contains full information
    ///   about checkout
    /// </summary>
    [Alias('pre_checkout_query')]
    PreCheckoutQuery: TtgPreCheckoutQuery;
    /// <summary>
    ///   Gets the update type.
    /// </summary>
    /// <value>
    ///   The update type.
    /// </value>
    /// <exception cref="System.ArgumentOutOfRangeException" />
    function &type: TtgUpdateType;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   Contains information about the current status of a webhook.
  /// </summary>
  TtgWebhookInfo = class
  public
    /// <summary>
    ///   Webhook URL, may be empty if webhook is not set up
    /// </summary>
    [Alias('url')]
    Url: string;
    /// <summary>
    ///   True, if a custom certificate was provided for webhook certificate
    ///   checks
    /// </summary>
    [Alias('has_custom_certificate')]
    HasCustomCertificate: Boolean;
    /// <summary>
    ///   Number of updates awaiting delivery
    /// </summary>
    [Alias('pending_update_count')]
    PendingUpdateCount: Integer;
    /// <summary>
    ///   Optional. Unix time for the most recent error that happened when
    ///   trying to deliver an update via webhook
    /// </summary>
    [Alias('last_error_date')]
    LastErrorDate: Integer;
    /// <summary>
    ///   Optional. Error message in human-readable format for the most recent
    ///   error that happened when trying to deliver an update via webhook
    /// </summary>
    [Alias('last_error_message')]
    LastErrorMessage: string;
    /// <summary>
    ///   Optional. Maximum allowed number of simultaneous HTTPS connections to
    ///   the webhook for update delivery
    /// </summary>
    [Alias('max_connections')]
    MaxConnections: Integer;
    /// <summary>
    ///   Optional. A list of update types the bot is subscribed to. Defaults
    ///   to all update types
    /// </summary>
    [Alias('allowed_updates')]
    AllowedUpdates: TList<string>;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TtgAnimation }
destructor TtgAnimation.Destroy;
begin
  FreeAndNil(Thumb);
  inherited;
end;

{TtgCallbackQuery}
destructor TtgCallbackQuery.Destroy;
begin
  FreeAndNil(From);
  FreeAndNil(message);
  inherited;
end;
{ TtgChatMember }

destructor TtgChatMember.Destroy;
begin
  FreeAndNil(User);
  inherited;
end;

{ TtgDocument }
destructor TtgDocument.Destroy;
begin
  FreeAndNil(Thumb);
  inherited;
end;

{ TtgFile }
function TtgFile.GetFileUrl(const AToken: string): string;
begin
  Result := 'https://api.telegram.org/file/bot' + AToken + '/' + FilePath;
end;
{ TtgFileToSend }

constructor TtgFileToSend.Create(const FileName: string; const Content: TStream);
begin
  Self.FileName := FileName;
  Self.Content := Content;
  DoCheckParams;
end;

destructor TtgFileToSend.Destroy;
begin
  FreeAndNil(Content);
  inherited;
end;

procedure TtgFileToSend.DoCheckParams;
begin
  if not Assigned(Content) then
  begin
    if not FileExists(FileName) then
      raise EFileNotFoundException.CreateFmt('File %S not found!', [FileName]);
  end
end;
{ TtgGame }

constructor TtgGame.Create;
begin
  inherited Create;
  Photo := TObjectList<TtgPhotoSize>.Create;
  TextEntities := TObjectList<TtgMessageEntity>.Create;
end;

destructor TtgGame.Destroy;
begin
  FreeAndNil(Photo);
  FreeAndNil(TextEntities);
  FreeAndNil(Animation);
  inherited;
end;

{ TtgGameHighScore }
destructor TtgGameHighScore.Destroy;
begin
  FreeAndNil(User);
  inherited;
end;

{ TtgInlineKeyboardButton }
constructor TtgInlineKeyboardButton.Create(const AText: string);
begin
  Text := AText;
end;

constructor TtgInlineKeyboardButton.Create(const AText, ACallbackData: string);
begin
  Self.Create(AText);
  Self.CallbackData := ACallbackData;
end;

constructor TtgInlineKeyboardButton.Create(const AText: string; ACallbackGame: TtgCallbackGame);
begin
  Self.Create(AText);
  Self.CallbackGame := ACallbackGame;
end;

 { TtgInlineQuery }
destructor TtgInlineQuery.Destroy;
begin
  FreeAndNil(From);
  inherited;
end;

{ TtgKeyboardButton }
constructor TtgKeyboardButton.Create(const AText: string; ARequestContact, ARequestLocation: Boolean);
begin
  Self.Text := AText;
  Self.RequestContact := ARequestContact;
  Self.RequestLocation := ARequestLocation;
end;

{ TtgMessage }

destructor TtgMessage.Destroy;
begin
  FreeAndNil(From);
  FreeAndNil(Chat);
  FreeAndNil(ForwardFrom);
  FreeAndNil(ForwardFromChat);
  FreeAndNil(ReplyToMessage);
  FreeAndNil(Audio);
  FreeAndNil(Document);
  FreeAndNil(Sticker);
  FreeAndNil(Video);
  FreeAndNil(Voice);
  FreeAndNil(Contact);
  FreeAndNil(Location);
  FreeAndNil(Venue);
  FreeAndNil(LeftChatMember);
  FreeAndNil(PinnedMessage);
  FreeAndNil(ForwardFromChat);
  FreeAndNil(NewChatMembers);
  FreeAndNil(Photo);
  FreeAndNil(Entities);
  FreeAndNil(NewChatPhoto);
  FreeAndNil(Game);
  inherited;
end;

{ TtgMessageEntity }
destructor TtgMessageEntity.Destroy;
begin
  FreeAndNil(User);
  inherited;
end;

{ TtgVenue }
destructor TtgVenue.Destroy;
begin
  FreeAndNil(Location);
  inherited;
end;

{ TtgVideo }
destructor TtgVideo.Destroy;
begin
  FreeAndNil(Thumb);
  inherited;
end;

{TtgShippingOption}
constructor TtgShippingOption.Create;
begin
  Prices := TObjectList<TtgLabeledPrice>.Create;
end;

destructor TtgShippingOption.Destroy;
begin
  FreeAndNil(Prices);
  inherited;
end;

{ TtgSticker }
destructor TtgSticker.Destroy;
begin
  FreeAndNil(Thumb);
  inherited;
end;

{ TtgUpdate }
function TtgUpdate.&type: TtgUpdateType;
begin
  if Assigned(message) then
    Exit(TtgUpdateType.MessageUpdate);
  if Assigned(InlineQuery) then
    Exit(TtgUpdateType.InlineQueryUpdate);
  if Assigned(ChosenInlineResult) then
    Exit(TtgUpdateType.ChosenInlineResultUpdate);
  if Assigned(CallbackQuery) then
    Exit(TtgUpdateType.CallbackQueryUpdate);
  if Assigned(EditedMessage) then
    Exit(TtgUpdateType.EditedMessage);
  if Assigned(ChannelPost) then
    Exit(TtgUpdateType.ChannelPost);
  if Assigned(EditedChannelPost) then
    Exit(TtgUpdateType.EditedChannelPost);
  if Assigned(ShippingQuery) then
    Exit(TtgUpdateType.ShippingQueryUpdate);
  if Assigned(PreCheckoutQuery) then
    Exit(TtgUpdateType.PreCheckoutQueryUpdate);
  Result := TtgUpdateType.UnknownUpdate;
end;

destructor TtgUpdate.Destroy;
begin
  FreeAndNil(message);
  FreeAndNil(EditedMessage);
  FreeAndNil(ChannelPost);
  FreeAndNil(EditedChannelPost);
  FreeAndNil(InlineQuery);
  FreeAndNil(ChosenInlineResult);
  FreeAndNil(CallbackQuery);
  inherited;
end;

{ TtgUserProfilePhotos }
constructor TtgUserProfilePhotos.Create;
begin
  Photos := TObjectList<TObjectList<TtgPhotoSize>>.Create();
end;

destructor TtgUserProfilePhotos.Destroy;
begin
  Photos.Free;
  inherited;
end;

{TtgWebhookInfo}
constructor TtgWebhookInfo.Create;
begin
  AllowedUpdates := TList<string>.Create;
end;

destructor TtgWebhookInfo.Destroy;
begin
  FreeAndNil(AllowedUpdates);
  inherited;
end;

{ TtgLocation }

constructor TtgLocation.Create(ALongitude, ALatitude: Single);
begin
  Longitude := ALongitude;
  Latitude := ALatitude;
end;

{ TtgApiResponse<T> }

destructor TtgApiResponse<T>.Destroy;
begin
  FreeAndNil(Parameters);
  inherited;
end;

end.

