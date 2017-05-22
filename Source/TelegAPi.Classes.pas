unit TelegAPi.Classes;

interface

uses
  XSuperObject,
  System.SysUtils,
  System.Classes;

type
  /// <summary>
  ///   Type of action to broadcast.
  /// </summary>
  /// <remarks>
  ///   We only recommend using this method when a response from the bot will
  ///   take a noticeable amount of time to arrive.
  /// </remarks>
  /// <example>
  ///   Example: The ImageBot needs some time to process a request and upload
  ///   the image. Instead of sending a text message along the lines of
  ///   “Retrieving image, please wait…”, the bot may use sendChatAction with
  ///   action = upload_photo. The user will see a “sending photo” status for
  ///   the bot.
  /// </example>
  TSendChatAction = (
    /// <summary>
    ///   for text messages
    /// </summary>
    Typing,
    /// <summary>
    ///   for photos
    /// </summary>
    Upload_photo,
    /// <summary>
    ///   for videos
    /// </summary>
    Record_video,
    /// <summary>
    ///   for videos
    /// </summary>
    Upload_video,
    /// <summary>
    ///   for audio files
    /// </summary>
    Record_audio,
    /// <summary>
    ///   for audio files
    /// </summary>
    Upload_audio,
    /// <summary>
    ///   for general files
    /// </summary>
    Upload_document,
    /// <summary>
    ///   for location data
    /// </summary>
    Find_location,
    /// <summary>
    ///   for video notes
    /// </summary>
    Record_video_note,
    /// <summary>
    ///   for video notes
    /// </summary>
    Upload_video_note );
{$SCOPEDENUMS ON}
  /// <summary>
  ///   The type of a Message
  /// </summary>
  TtgMessageType = ( UnknownMessage = 0, TextMessage, PhotoMessage,
    AudioMessage, VideoMessage, VoiceMessage, DocumentMessage, StickerMessage,
    LocationMessage, ContactMessage, ServiceMessage, VenueMessage );
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
  ///      ```text <br />pre-formatted fixed-width code block <br />```
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
  TtgParseMode = ( default = 0,
    /// <summary>
    ///   To use this mode, pass Markdown in the parse_mode field when using
    ///   sendMessage
    /// </summary>
    Markdown,
    /// <summary>
    ///   To use this mode, pass HTML in the parse_mode field when using
    ///   sendMessage
    /// </summary>
    Html );

  /// <summary>
  ///   The type of an Update
  /// </summary>
  TtgUpdateType = ( UnkownUpdate = 0, MessageUpdate, InlineQueryUpdate,
    ChosenInlineResultUpdate, CallbackQueryUpdate );
  TAllowedUpdate = ( message, Edited_message, Channel_post, Edited_channel_post,
    Inline_query, Chosen_inline_result, Callback_query );
  TAllowedUpdates = set of TAllowedUpdate;

const
  UPDATES_ALLOWED_ALL = [ low( TAllowedUpdate ) .. high( TAllowedUpdate ) ];

type
  ETelegramException = class( Exception );
  ETelegramTokenEmpty = class( ETelegramException );
  ETelegramDataConvert = class( ETelegramException );
  ETelegramUnknownData = class( ETelegramDataConvert );

  /// <summary>
  ///   This object represents a Telegram user or bot.
  /// </summary>
  [ Alias( 'User' ) ]
  TtgUser = class
    public
      /// <summary>
      ///   Unique identifier for this user or bot
      /// </summary>
      [ Alias( 'id' ) ]
      ID : Integer;
      /// <summary>
      ///   User‘s or bot’s first name
      /// </summary>
      [ Alias( 'first_name' ) ]
      FirstName : string;
      /// <summary>
      ///   Optional. User‘s or bot’s last name
      /// </summary>
      [ Alias( 'last_name' ) ]
      LastName : string;
      /// <summary>
      ///   Optional. User‘s or bot’s username
      /// </summary>
      [ Alias( 'username' ) ]
      Username : string;
      /// <summary>
      ///   Optional. IETF language tag of the user's language
      /// </summary>
      [ Alias( 'language_code' ) ]
      LanguageCode : string;
  end;

  /// <summary>
  ///   This object contains information about one member of the chat.
  /// </summary>
  [ Alias( 'ChatMember' ) ]
  TtgChatMember = class
    public
      /// <summary>
      ///   Information about the user
      /// </summary>
      [ Alias( 'user' ) ]
      User : TtgUser;
      /// <summary>
      ///   The member's status in the chat. Can be “creator”, “administrator”,
      ///   “member”, “left” or “kicked”
      /// </summary>
      [ Alias( 'status' ) ]
      Status : string;
      destructor Destroy; override;
  end;

  /// <summary>
  ///   This object represents a chat.
  /// </summary>
  [ Alias( 'Chat' ) ]
  TtgChat = class
    public
      /// <summary>
      ///   Unique identifier for this chat, not exceeding 1e13 by absolute
      ///   value
      /// </summary>
      [ Alias( 'id' ) ]
      ID : Int64;
      /// <summary>
      ///   Type of chat, can be either “private”, “group”, “supergroup” or
      ///   “channel”
      /// </summary>
      [ Alias( 'type' ) ]
      &Type : string;
      /// <summary>
      ///   Optional. Title, for channels and group chats
      /// </summary>
      [ Alias( 'title' ) ]
      Title : string;
      /// <summary>
      ///   Optional. Username, for private chats and channels if available
      /// </summary>
      [ Alias( 'username' ) ]
      Username : string;
      /// <summary>
      ///   Optional. First name of the other party in a private chat
      /// </summary>
      [ Alias( 'first_name' ) ]
      FirstName : string;
      /// <summary>
      ///   Optional. Last name of the other party in a private chat
      /// </summary>
      [ Alias( 'last_name' ) ]
      LastName : string;
      /// <summary>
      ///   Optional. True if a group has ‘All Members Are Admins’ enabled.
      /// </summary>
      [ Alias( 'all_members_are_administrators' ) ]
      AllMembersAreAdministrators : Boolean;
  end;

  /// <summary>
  ///   This object represents one special entity in a text message. For
  ///   example, hashtags, usernames, URLs, etc.
  /// </summary>
  [ Alias( 'MessageEntity' ) ]
  TtgMessageEntity = class
    public
      /// <summary>
      ///   Type of the entity. One of mention (@username), hashtag,
      ///   bot_command, url, email, bold (bold text), italic (italic text),
      ///   code (monowidth string), pre (monowidth block), text_link (for
      ///   clickable text URLs), text_mention (for users without usernames)
      /// </summary>
      [ Alias( 'type' ) ]
      &Type : string;
      /// <summary>
      ///   Offset in UTF-16 code units to the start of the entity
      /// </summary>
      [ Alias( 'offset' ) ]
      Offset : Integer;
      /// <summary>
      ///   Length of the entity in UTF-16 code units
      /// </summary>
      [ Alias( 'length' ) ]
      Length : Integer;
      /// <summary>
      ///   Optional. For “text_link” only, url that will be opened after user
      ///   taps on the text
      /// </summary>
      [ Alias( 'url' ) ]
      Url : string;
      /// <summary>
      ///   Optional. For “text_mention” only, the mentioned user
      /// </summary>
      [ Alias( 'user' ) ]
      User : TtgUser;
      destructor Destroy; override;
  end;

  [ Alias( 'File' ) ]
  TtgFile = class
    public
      [ Alias( 'file_id' ) ]
      FileId : string;
      [ Alias( 'file_size' ) ]
      FileSize : Integer;
      [ Alias( 'file_path' ) ]
      FilePath : string;
      function GetFileUrl( Const AToken : string ) : string;
  end;

  /// <summary>
  ///   This object represents an audio file to be treated as music by the
  ///   Telegram clients.
  /// </summary>
  [ Alias( 'Audio' ) ]
  TtgAudio = class( TtgFile )
    public
      /// <summary>
      ///   Duration of the audio in seconds as defined by sender
      /// </summary>
      [ Alias( 'duration' ) ]
      Duration : Integer;
      /// <summary>
      ///   Performer of the audio as defined by sender or by audio tags
      /// </summary>
      [ Alias( 'performer' ) ]
      Performer : string;
      /// <summary>
      ///   Title of the audio as defined by sender or by audio tags
      /// </summary>
      [ Alias( 'title' ) ]
      Title : string;
      /// <summary>
      ///   Optional. MIME type of the file as defined by sender
      /// </summary>
      [ Alias( 'mime_type' ) ]
      MimeType : string;
  end;

  /// <summary>
  ///   This object represents one size of a photo or a file/sticker thumbnail.
  /// </summary>
  /// <remarks>
  ///   A missing thumbnail for a file (or sticker) is presented as an empty
  ///   object.
  /// </remarks>
  [ Alias( 'PhotoSize' ) ]
  TtgPhotoSize = class( TtgFile )
    public
      /// <summary>
      ///   Photo width
      /// </summary>
      [ Alias( 'width' ) ]
      Width : Integer;
      /// <summary>
      ///   Photo height
      /// </summary>
      [ Alias( 'Height' ) ]
      Height : Integer;
  end;

  /// <summary>
  ///   This object represents a general file (as opposed to photos, voice
  ///   messages and audio files).
  /// </summary>
  [ Alias( 'Document' ) ]
  TtgDocument = class( TtgFile )
    public
      /// <summary>
      ///   Document thumbnail as defined by sender
      /// </summary>
      [ Alias( 'thumb' ) ]
      Thumb : TtgPhotoSize;
      /// <summary>
      ///   Optional. Original filename as defined by sender
      /// </summary>
      [ Alias( 'file_name' ) ]
      FileName : string;
      /// <summary>
      ///   Optional. MIME type of the file as defined by sender
      /// </summary>
      [ Alias( 'mime_type' ) ]
      MimeType : string;
      destructor Destroy; override;
  end;

  /// <summary>
  ///   This object represents a sticker.
  /// </summary>
  [ Alias( 'Sticker' ) ]
  TtgSticker = class( TtgFile )
    public
      /// <summary>
      ///   Sticker width
      /// </summary>
      [ Alias( 'width' ) ]
      Width : Integer;
      /// <summary>
      ///   Sticker height
      /// </summary>
      [ Alias( 'width' ) ]
      Height : Integer;
      /// <summary>
      ///   Sticker thumbnail in .webp or .jpg format
      /// </summary>
      [ Alias( 'thumb' ) ]
      Thumb : TtgPhotoSize;
      /// <summary>
      ///   Optional. Emoji associated with the sticker
      /// </summary>
      [ Alias( 'emoji' ) ]
      Emoji : string;
      destructor Destroy; override;
  end;

  /// <summary>
  ///   This object represents a video file.
  /// </summary>
  [ Alias( 'Video' ) ]
  TtgVideo = class( TtgFile )
    public
      /// <summary>
      ///   Video width as defined by sender
      /// </summary>
      [ Alias( 'width' ) ]
      Width : Integer;
      /// <summary>
      ///   Video height as defined by sender
      /// </summary>
      [ Alias( 'height' ) ]
      Height : Integer;
      /// <summary>
      ///   Duration of the video in seconds as defined by sender
      /// </summary>
      [ Alias( 'duration' ) ]
      Duration : Integer;
      /// <summary>
      ///   Video thumbnail
      /// </summary>
      [ Alias( 'thumb' ) ]
      Thumb : TtgPhotoSize;
      /// <summary>
      ///   Optional. Mime type of a file as defined by sender
      /// </summary>
      [ Alias( 'mime_type' ) ]
      MimeType : string;
      destructor Destroy; override;
  end;

  /// <summary>
  ///   This object represents a video message
  /// </summary>
  /// <remarks>
  ///   available in Telegram apps as of v.4.0
  /// </remarks>
  [ Alias( 'VideoNote' ) ]
  TtgVideoNote = class
    public
      /// <summary>
      ///   Unique identifier for this file
      /// </summary>
      [ Alias( 'file_id' ) ]
      File_id : string;
      /// <summary>
      ///   Video width and height as defined by sender
      /// </summary>
      [ Alias( 'length' ) ]
      Length : Integer;
      /// <summary>
      ///   Duration of the video in seconds as defined by sender
      /// </summary>
      [ Alias( 'duration' ) ]
      Duration : Integer;
      /// <summary>
      ///   Optional. Video thumbnail
      /// </summary>
      [ Alias( 'thumb' ) ]
      Thumb : TtgPhotoSize;
      /// <summary>
      ///   Optional. File size
      /// </summary>
      [ Alias( 'file_size' ) ]
      File_size : Integer;
  end;

  /// <summary>
  ///   This object represents a voice note.
  /// </summary>
  [ Alias( 'Voice' ) ]
  TtgVoice = class( TtgFile )
    public
      /// <summary>
      ///   Duration of the audio in seconds as defined by sender
      /// </summary>
      [ Alias( 'duration' ) ]
      Duration : Integer;
      /// <summary>
      ///   Optional. MIME type of the file as defined by sender
      /// </summary>
      [ Alias( 'mime_type' ) ]
      MimeType : string;
  end;

  /// <summary>
  ///   This object represents a phone contact.
  /// </summary>
  [ Alias( 'Contact' ) ]
  TtgContact = class
    public
      /// <summary>
      ///   Contact's phone number
      /// </summary>
      [ Alias( 'phone_number' ) ]
      PhoneNumber : string;
      /// <summary>
      ///   Contact's first name
      /// </summary>
      [ Alias( 'first_name' ) ]
      FirstName : string;
      /// <summary>
      ///   Optional. Contact's last name
      /// </summary>
      [ Alias( 'last_name' ) ]
      LastName : string;
      /// <summary>
      ///   Optional. Contact's user identifier in Telegram
      /// </summary>
      [ Alias( 'user_id' ) ]
      UserId : Integer;
  end;

  /// <summary>
  ///   This object represents a point on the map.
  /// </summary>
  [ Alias( 'Location' ) ]
  TtgLocation = class
    public
      /// <summary>
      ///   Longitude as defined by sender
      /// </summary>
      [ Alias( 'longitude' ) ]
      Longitude : Single;
      /// <summary>
      ///   Latitude as defined by sender
      /// </summary>
      [ Alias( 'latitude' ) ]
      Latitude : Single;
  end;

  /// <summary>
  ///   This object represents a venue.
  /// </summary>
  [ Alias( 'Venue' ) ]
  TtgVenue = class
    public
      /// <summary>
      ///   Venue location
      /// </summary>
      [ Alias( 'location' ) ]
      Location : TtgLocation;
      /// <summary>
      ///   Title of the result
      /// </summary>
      [ Alias( 'title' ) ]
      Title : string;
      /// <summary>
      ///   Address of the venue
      /// </summary>
      [ Alias( 'address' ) ]
      Address : string;
      /// <summary>
      ///   Optional. Foursquare identifier of the venue
      /// </summary>
      [ Alias( 'foursquare_id' ) ]
      FoursquareId : string;
      destructor Destroy; override;
  end;

  /// <summary>
  ///   You can provide an animation for your game so that it looks stylish in
  ///   chats (check out Lumberjack for an example). This object represents an
  ///   animation file to be displayed in the message containing a game.
  /// </summary>
  [ Alias( 'Animation' ) ]
  TtgAnimation = class
    public
      /// <summary>
      ///   Unique file identifier
      /// </summary>
      [ Alias( 'file_id' ) ]
      FileId : string;
      /// <summary>
      ///   Optional. Animation thumbnail as defined by sender
      /// </summary>
      [ Alias( 'thumb' ) ]
      Thumb : TtgPhotoSize;
      /// <summary>
      ///   Optional. Original animation filename as defined by sender
      /// </summary>
      [ Alias( 'file_name' ) ]
      FileName : string;
      /// <summary>
      ///   Optional. MIME type of the file as defined by sender
      /// </summary>
      [ Alias( 'mime_type' ) ]
      MimeType : string;
      /// <summary>
      ///   Optional. File size
      /// </summary>
      [ Alias( 'file_size' ) ]
      FileSize : Integer;
      destructor Destroy; override;
  end;

  /// <summary>
  ///   This object represents one row of the high scores table for a game.
  /// </summary>
  [ Alias( 'Game' ) ]
  TtgGameHighScore = class
    public
      /// <summary>
      ///   Position in high score table for the game
      /// </summary>
      Position : Integer;
      /// <summary>
      ///   User
      /// </summary>
      User : TtgUser;
      /// <summary>
      ///   Score
      /// </summary>
      Score : Integer;
      destructor Destroy; override;
  end;

  /// <summary>
  ///   This object represents a game. Use BotFather to create and edit games,
  ///   their short names will act as unique identifiers.
  /// </summary>
  [ Alias( 'Game' ) ]
  TtgGame = class
    public
      /// <summary>
      ///   Title of the game
      /// </summary>
      [ Alias( 'title' ) ]
      Title : string;
      /// <summary>
      ///   Description of the game
      /// </summary>
      [ Alias( 'description' ) ]
      Description : string;
      /// <summary>
      ///   Photo that will be displayed in the game message in chats.
      /// </summary>
      [ Alias( 'photo' ) ]
      Photo : TArray< TtgPhotoSize >;
      /// <summary>
      ///   Optional. Brief description of the game or high scores included in
      ///   the game message. Can be automatically edited to include current
      ///   high scores for the game when the bot calls setGameScore, or
      ///   manually edited using editMessageText. 0-4096 characters.
      /// </summary>
      [ Alias( 'text' ) ]
      Text : string;
      /// <summary>
      ///   Optional. Special entities that appear in text, such as usernames,
      ///   URLs, bot commands, etc.
      /// </summary>
      [ Alias( 'text_entities' ) ]
      Text_entities : TArray< TtgMessageEntity >;
      /// <summary>
      ///   Optional. Animation that will be displayed in the game message in
      ///   chats. Upload via BotFather
      /// </summary>
      [ Alias( 'animation' ) ]
      Animation : TtgAnimation;
      destructor Destroy; override;
  end;

  /// <summary>
  ///   This object represents a message.
  /// </summary>
  [ Alias( 'Message' ) ]
  TtgMessage = class
    public
      /// <summary>
      ///   Unique message identifier
      /// </summary>
      [ Alias( 'message_id' ) ]
      MessageId : Integer;
      /// <summary>
      ///   Sender
      /// </summary>
      [ Alias( 'from' ) ]
      From : TtgUser;
      /// <summary>
      ///   Date the message was sent in Unix time
      /// </summary>
      [ Alias( 'date' ) ]
      Date : Integer;
      /// <summary>
      ///   Conversation the message belongs to
      /// </summary>
      [ Alias( 'chat' ) ]
      Chat : TtgChat;
      /// <summary>
      ///   Optional. For forwarded messages, sender of the original message
      /// </summary>
      [ Alias( 'forward_from' ) ]
      ForwardFrom : TtgUser;
      /// <summary>
      ///   Optional. For messages forwarded from a channel, information about
      ///   the original channel
      /// </summary>
      [ Alias( 'forward_from_chat' ) ]
      ForwardFromChat : TtgChat;
      /// <summary>
      ///   Optional. For forwarded messages, date the original message was
      ///   sent in Unix time
      /// </summary>
      [ Alias( 'forward_date' ) ]
      ForwardDate : Integer;
      /// <summary>
      ///   Optional. For replies, the original message. Note that the Message
      ///   object in this field will not contain further reply_to_message
      ///   fields even if it itself is a reply.
      /// </summary>
      [ Alias( 'reply_to_message' ) ]
      ReplyToMessage : TtgMessage;
      /// <summary>
      ///   Optional. Date the message was last edited in Unix time.
      /// </summary>
      [ Alias( 'edit_date' ) ]
      EditDate : Integer;
      /// <summary>
      ///   Optional. For text messages, the actual UTF-8 text of the message
      /// </summary>
      [ Alias( 'text' ) ]
      Text : string;
      /// <summary>
      ///   Optional. For text messages, special entities like usernames, URLs,
      ///   bot commands, etc. that appear in the text
      /// </summary>
      [ Alias( 'entities' ) ]
      Entities : TArray< TtgMessageEntity >;
      /// <summary>
      ///   Optional. Message is an audio file, information about the file
      /// </summary>
      [ Alias( 'audio' ) ]
      Audio : TtgAudio;
      /// <summary>
      ///   Optional. Message is a general file, information about the file
      /// </summary>
      [ Alias( 'document' ) ]
      Document : TtgDocument;
      /// <summary>
      ///   Optional. Message is a game, information about the game.
      /// </summary>
      [ Alias( 'game' ) ]
      Game : TtgGame;
      /// <summary>
      ///   Optional. Message is a photo, available sizes of the photo
      /// </summary>
      [ Alias( 'photo' ) ]
      Photo : TArray< TtgPhotoSize >;
      /// <summary>
      ///   Optional. Message is a sticker, information about the sticker
      /// </summary>
      [ Alias( 'sticker' ) ]
      Sticker : TtgSticker;
      /// <summary>
      ///   Optional. Message is a video, information about the video
      /// </summary>
      [ Alias( 'video' ) ]
      Video : TtgVideo;
      /// <summary>
      ///   Message is a voice message, information about the file
      /// </summary>
      [ Alias( 'voice' ) ]
      Voice : TtgVoice;

      /// <summary>
      ///   Optional. Message is a video note, information about the video
      ///   message
      /// </summary>
      [ Alias( 'video_note' ) ]
      Video_note : TtgVideoNote;

      /// <summary>
      ///   Optional. Caption for the document, photo or video, 0-200
      ///   characters
      /// </summary>
      [ Alias( 'caption' ) ]
      Caption : string;
      /// <summary>
      ///   Optional. Message is a shared contact, information about the
      ///   contact
      /// </summary>
      [ Alias( 'contact' ) ]
      Contact : TtgContact;
      /// <summary>
      ///   Optional. Message is a shared location, information about the
      ///   location
      /// </summary>
      [ Alias( 'location' ) ]
      Location : TtgLocation;
      /// <summary>
      ///   Optional. Message is a venue, information about the venue
      /// </summary>
      [ Alias( 'venue' ) ]
      Venue : TtgVenue;
      /// <summary>
      ///   Optional. New members that were added to the group or supergroup
      ///   and information about them (the bot itself may be one of these
      ///   members)
      /// </summary>
      [ Alias( 'new_chat_members' ) ]
      NewChatMembers : TArray< TtgUser >;
      /// <summary>
      ///   Optional. A member was removed from the group, information about
      ///   them (this member may be bot itself)
      /// </summary>
      [ Alias( 'left_chat_member' ) ]
      LeftChatMember : TtgUser;
      /// <summary>
      ///   Optional. A group title was changed to this value
      /// </summary>
      [ Alias( 'new_chat_title' ) ]
      NewChatTitle : string;
      /// <summary>
      ///   Optional. A group photo was change to this value
      /// </summary>
      [ Alias( 'new_chat_photo' ) ]
      NewChatPhoto : TArray< TtgPhotoSize >;
      /// <summary>
      ///   Optional. Informs that the group photo was deleted
      /// </summary>
      [ Alias( 'delete_chat_photo' ) ]
      DeleteChatPhoto : Boolean;
      /// <summary>
      ///   Optional. Informs that the group has been created
      /// </summary>
      [ Alias( 'group_chat_created' ) ]
      GroupChatCreated : Boolean;
      /// <summary>
      ///   Optional. Service message: the supergroup has been created
      /// </summary>
      [ Alias( 'supergroup_chat_created' ) ]
      SupergroupChatCreated : Boolean;
      /// <summary>
      ///   Optional. Service message: the channel has been created
      /// </summary>
      [ Alias( 'channel_chat_created' ) ]
      ChannelChatCreated : Boolean;
      /// <summary>
      ///   Optional. The group has been migrated to a supergroup with the
      ///   specified identifier
      /// </summary>
      [ Alias( 'migrate_to_chat_id' ) ]
      MigrateToChatId : Int64;
      /// <summary>
      ///   Optional. The supergroup has been migrated from a group with the
      ///   specified identifier
      /// </summary>
      [ Alias( 'migrate_from_chat_id' ) ]
      MigrateFromChatId : Int64;
      /// <summary>
      ///   Optional. Specified message was pinned. Note that the Message
      ///   object in this field will not contain further reply_to_message
      ///   fields even if it is itself a reply
      /// </summary>
      [ Alias( 'pinned_message' ) ]
      PinnedMessage : TtgMessage;
      destructor Destroy; override;
  end;

  /// <summary>
  ///   This object represent a user's profile pictures.
  /// </summary>
  [ Alias( 'UserProfilePhotos' ) ]
  TtgUserProfilePhotos = class
    public
      /// <summary>
      ///   Total number of profile pictures the target user has
      /// </summary>
      [ Alias( 'total_count' ) ]
      TotalCount : Integer;
      /// <summary>
      ///   Requested profile pictures (in up to 4 sizes each)
      /// </summary>
      [ Alias( 'photos' ) ]
      Photos : TArray< TArray< TtgPhotoSize > >;
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
  [ Alias( 'KeyboardButton' ) ]
  TtgKeyboardButton = class
    public
      /// <summary>
      ///   Text of the button. If none of the optional fields are used, it
      ///   will be sent to the bot as a message when the button is pressed
      /// </summary>
      [ Alias( 'text' ) ]
      Text : string;
      /// <summary>
      ///   Optional. If True, the user's phone number will be sent as a
      ///   contact when the button is pressed. Available in private chats only
      /// </summary>
      [ Alias( 'request_contact' ) ]
      Request_contact : Boolean;
      /// <summary>
      ///   Optional. If True, the user's current location will be sent when
      ///   the button is pressed. Available in private chats only
      /// </summary>
      [ Alias( 'request_location' ) ]
      Request_location : Boolean;
      constructor Create(
        const Text       : string;
        Request_contact  : Boolean = False;
        Request_location : Boolean = False ); overload;
  end;

  TtgReplyMarkup = class
    public
      /// <summary>
      ///   Optional. Use this parameter if you want to force reply from
      ///   specific users only. Targets: 1) users that are @mentioned in the
      ///   text of the Message object; 2) if the bot's message is a reply (has
      ///   reply_to_message_id), sender of the original message.
      /// </summary>
      [ Alias( 'selective' ) ]
      Selective : Boolean;
  end;

  /// <summary>
  ///   Upon receiving a message with this object, Telegram clients will
  ///   display a reply interface to the user (act as if the user has selected
  ///   the bot‘s message and tapped ’Reply'). This can be extremely useful if
  ///   you want to create user-friendly step-by-step interfaces without having
  ///   to sacrifice privacy mode.
  /// </summary>
  [ Alias( 'ForceReply' ) ]
  TtgForceReply = class( TtgReplyMarkup )
    public
      /// <summary>
      ///   Shows reply interface to the user, as if they manually selected the
      ///   bot‘s message and tapped ’Reply'
      /// </summary>
      [ Alias( 'force_reply' ) ]
      Force_reply : Boolean;
  end;

  /// <summary>
  ///   Upon receiving a message with this object, Telegram clients will hide
  ///   the current custom keyboard and display the default letter-keyboard. By
  ///   default, custom keyboards are displayed until a new keyboard is sent by
  ///   a bot. An exception is made for one-time keyboards that are hidden
  ///   immediately after the user presses a button (see ReplyKeyboardMarkup).
  /// </summary>
  [ Alias( 'ReplyKeyboardHide' ) ]
  TtgReplyKeyboardHide = class( TtgReplyMarkup )
    public
      /// <summary>
      ///   Requested profile pictures (in up to 4 sizes each)
      /// </summary>
      [ Alias( 'hide_keyboard' ) ]
      Hide_keyboard : Boolean;
  end;

  /// <summary>
  ///   This object represents a custom keyboard with reply options (see
  ///   Introduction to bots for details and examples).
  /// </summary>
  [ Alias( 'ReplyKeyboardMarkup' ) ]
  TtgReplyKeyboardMarkup = class( TtgReplyMarkup )
    private
      function GetRowCount : Integer;
      procedure SetRowCount( const Value : Integer );
      function GetColCount : Integer;
      procedure SetColCount( const Value : Integer );
    public
      destructor Destroy; override;
      property RowCount : Integer
        read GetRowCount
        write SetRowCount;
      property ColCount : Integer
        read GetColCount
        write SetColCount;
    public
      /// <summary>
      ///   Array of button rows, each represented by an Array of
      ///   KeyboardButton objects
      /// </summary>
      [ Alias( 'keyboard' ) ]
      KeyBoard : TArray< TArray< TtgKeyboardButton > >;
      /// <summary>
      ///   Optional. Requests clients to resize the keyboard vertically for
      ///   optimal fit (e.g., make the keyboard smaller if there are just two
      ///   rows of buttons). Defaults to false, in which case the custom
      ///   keyboard is always of the same height as the app's standard
      ///   keyboard.
      /// </summary>
      [ Alias( 'resize_keyboard' ) ]
      Resize_keyboard : Boolean;
      /// <summary>
      ///   Optional. Requests clients to hide the keyboard as soon as it's
      ///   been used. The keyboard will still be available, but clients will
      ///   automatically display the usual letter-keyboard in the chat – the
      ///   user can press a special button in the input field to see the
      ///   custom keyboard again. Defaults to false.
      /// </summary>
      [ Alias( 'one_time_keyboard' ) ]
      One_time_keyboard : Boolean;
  end;

  /// <summary>
  ///   This object represents one button of an inline keyboard. You must use
  ///   exactly one of the optional fields.
  /// </summary>
  [ Alias( 'InlineKeyboardButton' ) ]
  TtgInlineKeyboardButton = class
    public
      /// <summary>
      ///   Label text on the button
      /// </summary>
      // [DISABLE]
      [ Alias( 'text' ) ]
      Text : string;
      /// <summary>
      ///   Optional. HTTP url to be opened when button is pressed
      /// </summary>
      [ Alias( 'url' ) ]
      Url : string;
      /// <summary>
      ///   Optional. Data to be sent in a callback query to the bot when
      ///   button is pressed, 1-64 bytes
      /// </summary>
      [ Alias( 'callback_data' ) ]
      Callback_data : string;
      /// <summary>
      ///   Optional. If set, pressing the button will prompt the user to
      ///   select one of their chats, open that chat and insert the bot‘s
      ///   username and the specified inline query in the input field. Can be
      ///   empty, in which case just the bot’s username will be inserted.
      /// </summary>
      /// <remarks>
      ///   Note: This offers an easy way for users to start using your bot in
      ///   inline mode when they are currently in a private chat with it.
      ///   Especially useful when combined with switch_pm… actions – in this
      ///   case the user will be automatically returned to the chat they
      ///   switched from, skipping the chat selection screen.
      /// </remarks>
      [ Alias( 'switch_inline_query' ) ]
      Switch_inline_query : string;
      /// <summary>
      ///   Optional. If set, pressing the button will insert the bot‘s
      ///   username and the specified inline query in the current chat's input
      ///   field. Can be empty, in which case only the bot’s username will be
      ///   inserted. <br /><br /> This offers a quick way for the user to open
      ///   your bot in inline mode in the same chat – good for selecting
      ///   something from multiple options.
      /// </summary>
      [ Alias( 'switch_inline_query_current_chat' ) ]
      Switch_inline_query_current_chat : string;
      /// <summary>
      ///   Optional. Description of the game that will be launched when the
      ///   user presses the button. <br /><br />
      /// </summary>
      /// <remarks>
      ///   NOTE: This type of button must always be the first button in the
      ///   first row.
      /// </remarks>
      [ Alias( 'callback_game' ) ]
      Callback_game : string;
      /// <summary>
      ///   Optional. Specify True, to send a Pay button. <br /><br />
      /// </summary>
      /// <remarks>
      ///   NOTE: This type of button must always be the first button in the
      ///   first row.
      /// </remarks>
      [ Alias( 'pay' ) ]
      Pay : Boolean;
  end;

  /// <summary>
  ///   This object represents an inline keyboard that appears right next to
  ///   the message it belongs to.
  /// </summary>
  /// <remarks>
  ///   Warning: Inline keyboards are currently being tested and are only
  ///   available in one-on-one chats (i.e., user-bot or user-user in the case
  ///   of inline bots).
  /// </remarks>
  [ Alias( 'InlineKeyboardMarkup' ) ]
  TtgInlineKeyboardMarkup = class( TtgReplyMarkup )
    public
      /// <summary>
      ///   Array of button rows, each represented by an Array of
      ///   InlineKeyboardButton objects
      /// </summary>
      [ Alias( 'inline_keyboard' ) ]
      Inline_keyboard : TArray< TArray< TtgInlineKeyboardButton > >;
  end;

  [ Alias( '' ) ]
  TtgApiResponse< T > = class
    public
      /// <summary>
      ///   Gets a value indicating whether the request was successful.
      /// </summary>
      [ Alias( 'ok' ) ]
      Ok : Boolean;
      /// <summary>
      ///   Gets the result object.
      /// </summary>
      /// <value>
      ///   The result object.
      /// </value>
      [ Alias( 'result' ) ]
      ResultObject : T;
      /// <summary>
      ///   Gets the error message.
      /// </summary>
      /// <value>
      ///   The error message.
      /// </value>
      [ Alias( 'description' ) ]
      Message : string;
      /// <summary>
      ///   Gets the error code.
      /// </summary>
      /// <value>
      ///   The error code
      /// </value>
      [ Alias( 'error_code' ) ]
      Code : Integer;
  end;

  [ Alias( 'FileToSend' ) ]
  TtgFileToSend = class
    public
      FileName : string;
      Content : TStream;
      constructor Create(
        const FileName : string;
        const Content  : TStream );
      destructor Destroy; override;
  end;

  /// <summary>
  ///   This object represents an incoming inline query. When the user sends an
  ///   empty query, your bot could return some default or trending results.
  /// </summary>
  [ Alias( 'InlineQuery' ) ]
  TtgInlineQuery = class
    private
      FID : string;
      FFrom : TtgUser;
      FQuery : string;
      Foffset : string;
    public
      destructor Destroy; override;
    published
      /// <summary>
      ///   Unique identifier for this query
      /// </summary>
      [ Alias( 'id' ) ]
      property ID : string
        read FID
        write FID;
      /// <summary>
      ///   Sender
      /// </summary>
      [ Alias( 'from' ) ]
      property From : TtgUser
        read FFrom
        write FFrom;
      /// <summary>
      ///   Text of the query
      /// </summary>
      [ Alias( 'query' ) ]
      property Query : string
        read FQuery
        write FQuery;
      /// <summary>
      ///   Offset of the results to be returned, can be controlled by the bot
      /// </summary>
      [ Alias( 'offset' ) ]
      property Offset : string
        read Foffset
        write Foffset;
  end;

  /// <summary>
  ///   Represents a result of an inline query that was chosen by the user and
  ///   sent to their chat partner.
  /// </summary>
  [ Alias( 'ChosenInlineResult' ) ]
  TtgChosenInlineResult = class
    private
      FResultId : string;
      FFrom : TtgUser;
      FLocation : TtgLocation;
      FQuery : string;
      Finline_message_id : string;
    published
      /// <summary>
      ///   The unique identifier for the result that was chosen.
      /// </summary>
      [ Alias( 'result_id' ) ]
      property ResultId : string
        read FResultId
        write FResultId;
      /// <summary>
      ///   The user that chose the result.
      /// </summary>
      [ Alias( 'from' ) ]
      property From : TtgUser
        read FFrom
        write FFrom;
      /// <summary>
      ///   The query that was used to obtain the result.
      /// </summary>
      [ Alias( 'location' ) ]
      property Location : TtgLocation
        read FLocation
        write FLocation;
      /// <summary>
      ///   Optional. Identifier of the sent inline message. Available only if
      ///   there is an inline keyboard attached to the message. Will be also
      ///   received in callback queries and can be used to edit the message.
      /// </summary>
      [ Alias( 'inline_message_id' ) ]
      property Inline_message_id : string
        read Finline_message_id
        write Finline_message_id;
      /// <summary>
      ///   The query that was used to obtain the result.
      /// </summary>
      [ Alias( 'query' ) ]
      property Query : string
        read FQuery
        write FQuery;
  end;

  [ Alias( 'CallbackQuery' ) ]
  TtgCallbackQuery = class
    private
      FID : string;
      FFrom : TtgUser;
      FMessage : TtgMessage;
      FInlineMessageId : string;
      FData : string;
    published
      /// <summary>
      ///   Unique identifier for this query
      /// </summary>
      [ Alias( 'id' ) ]
      property ID : string
        read FID
        write FID;
      /// <summary>
      ///   Sender
      /// </summary>
      [ Alias( 'from' ) ]
      property From : TtgUser
        read FFrom
        write FFrom;
      /// <summary>
      ///   Optional. Message with the callback button that originated the
      ///   query. Note that message content and message date will not be
      ///   available if the message is too old
      /// </summary>
      [ Alias( 'message' ) ]
      property message : TtgMessage
        read FMessage
        write FMessage;
      /// <summary>
      ///   Optional. Identifier of the message sent via the bot in inline
      ///   mode, that originated the query
      /// </summary>
      [ Alias( 'inline_message_id' ) ]
      property InlineMessageId : string
        read FInlineMessageId
        write FInlineMessageId;
      /// <summary>
      ///   Data associated with the callback button. Be aware that a bad
      ///   client can send arbitrary data in this field
      /// </summary>
      [ Alias( 'data' ) ]
      property Data : string
        read FData
        write FData;
  end;

  /// <summary>
  ///   =This object represents an incoming update.
  /// </summary>
  /// <remarks>
  ///   Only one of the optional parameters can be present in any given update.
  /// </remarks>
  [ Alias( 'Update' ) ]
  TtgUpdate = class
    public
      destructor Destroy; override;
    public
      /// <summary>
      ///   The update‘s unique identifier. Update identifiers start from a
      ///   certain positive number and increase sequentially. This ID becomes
      ///   especially handy if you’re using Webhooks, since it allows you to
      ///   ignore repeated updates or to restore the correct update sequence,
      ///   should they get out of order.
      /// </summary>
      [ Alias( 'update_id' ) ]
      ID : Integer;
      /// <summary>
      ///   Optional. New incoming message of any kind — text, photo, sticker,
      ///   etc.
      /// </summary>
      [ Alias( 'message' ) ]
      Message : TtgMessage;
      /// <summary>
      ///   Optional. New version of a message that is known to the bot and was
      ///   edited
      /// </summary>
      [ Alias( 'edited_message' ) ]
      EditedMessage : TtgMessage;
      /// <summary>
      ///   Optional. New incoming channel post of any kind — text, photo,
      ///   sticker, etc.
      /// </summary>
      [ Alias( 'channel_post' ) ]
      Channel_post : TtgMessage;
      /// <summary>
      ///   Optional. New version of a channel post that is known to the bot
      ///   and was edited
      /// </summary>
      [ Alias( 'edited_channel_post' ) ]
      Edited_channel_post : TtgMessage;
      /// <summary>
      ///   Optional. New incoming inline query
      /// </summary>
      [ Alias( 'inline_query' ) ]
      InlineQuery : TtgInlineQuery;
      /// <summary>
      ///   Optional. The result of a inline query that was chosen by a user
      ///   and sent to their chat partner
      /// </summary>
      [ Alias( 'chosen_inline_result' ) ]
      ChosenInlineResult : TtgChosenInlineResult;
      /// <summary>
      ///   Optional. New incoming callback query
      /// </summary>
      [ Alias( 'callback_query' ) ]
      CallbackQuery : TtgCallbackQuery;
  end;

  /// <summary>
  ///   This object represents the content of a message to be sent as a result
  ///   of an inline query.
  /// </summary>
  [ Alias( 'InputMessageContent' ) ]
  TtgInputMessageContent = class

  end;

  /// <summary>
  ///   Represents the content of a text message to be sent as the result of an
  ///   inline query.
  /// </summary>
  [ Alias( 'InputTextMessageContent' ) ]
  TtgInputTextMessageContent = class( TtgInputMessageContent )
    private
      Fmessage_text : string;
      Fparse_mode : string;
      Fdisable_web_page_preview : Boolean;
    published
      /// <summary>
      ///   Text of the message to be sent, 1-4096 characters
      /// </summary>
      [ Alias( 'message_text' ) ]
      property Message_text : string
        read Fmessage_text
        write Fmessage_text;
      /// <summary>
      ///   Optional. Send Markdown or HTML, if you want Telegram apps to show
      ///   bold, italic, fixed-width text or inline URLs in your bot's
      ///   message.
      /// </summary>
      [ Alias( 'parse_mode' ) ]
      property Parse_mode : string
        read Fparse_mode
        write Fparse_mode;
      /// <summary>
      ///   Optional. Disables link previews for links in the sent message
      /// </summary>
      [ Alias( 'disable_web_page_preview' ) ]
      property Disable_web_page_preview : Boolean
        read Fdisable_web_page_preview
        write Fdisable_web_page_preview;
  end;

  /// <summary>
  ///   Represents the content of a location message to be sent as the result
  ///   of an inline query.
  /// </summary>
  [ Alias( 'InputLocationMessageContent' ) ]
  TtgInputLocationMessageContent = class( TtgInputMessageContent )
    private
      FLatitude : Single;
      FLongitude : Single;
    published
      /// <summary>
      ///   Latitude of the location in degrees
      /// </summary>
      [ Alias( 'latitude' ) ]
      property Latitude : Single
        read FLatitude
        write FLatitude;
      /// <summary>
      ///   Longitude of the location in degrees
      /// </summary>
      [ Alias( 'longitude' ) ]
      property Longitude : Single
        read FLongitude
        write FLongitude;
  end;

  /// <summary>
  ///   Represents the content of a venue message to be sent as the result of
  ///   an inline query.
  /// </summary>
  [ Alias( 'InputVenueMessageContent' ) ]
  TtgInputVenueMessageContent = class( TtgInputMessageContent )
    private
      FLatitude : Single;
      FLongitude : Single;
      Ftitle : string;
      FAddress : string;
      Ffoursquare_id : string;
    published
      /// <summary>
      ///   Latitude of the venue in degrees
      /// </summary>
      [ Alias( 'latitude' ) ]
      property Latitude : Single
        read FLatitude
        write FLatitude;
      /// <summary>
      ///   Longitude of the venue in degrees
      /// </summary>
      [ Alias( 'longitude' ) ]
      property Longitude : Single
        read FLongitude
        write FLongitude;
      /// <summary>
      ///   Name of the venue
      /// </summary>
      [ Alias( 'title' ) ]
      property Title : string
        read Ftitle
        write Ftitle;
      /// <summary>
      ///   Address of the venue
      /// </summary>
      [ Alias( 'address' ) ]
      property Address : string
        read FAddress
        write FAddress;
      /// <summary>
      ///   Optional. Foursquare identifier of the venue, if known
      /// </summary>
      [ Alias( 'foursquare_id' ) ]
      property Foursquare_id : string
        read Ffoursquare_id
        write Ffoursquare_id;
  end;

  /// <summary>
  ///   Represents the content of a contact message to be sent as the result of
  ///   an inline query.
  /// </summary>
  [ Alias( 'InputContactMessageContent' ) ]
  TtgInputContactMessageContent = class( TtgInputMessageContent )
    private
      Fphone_number : string;
      Ffirst_name : string;
      Flast_name : string;
    published
      /// <summary>
      ///   Contact's phone number
      /// </summary>
      [ Alias( 'phone_number' ) ]
      property Phone_number : string
        read Fphone_number
        write Fphone_number;
      /// <summary>
      ///   Contact's first name
      /// </summary>
      [ Alias( 'first_name' ) ]
      property First_name : string
        read Ffirst_name
        write Ffirst_name;
      /// <summary>
      ///   Optional. Contact's last name
      /// </summary>
      [ Alias( 'last_name' ) ]
      property Last_name : string
        read Flast_name
        write Flast_name;
  end;

  /// <summary />
  [ Alias( '' ) ]
  /// <summary>This object represents one result of an inline query. Telegram clients currently support results of the following 19 types   /// </summary>
  [ Alias( 'InlineQueryResult' ) ]
  TtgInlineQueryResult = class
    private
      Ftype : string;
      FID : string;
      Freply_markup : TtgInlineKeyboardMarkup;
      Finput_message_content : TtgInputMessageContent;
    public
      destructor Destroy; override;
    published
      /// <summary>
      ///   Type of the result
      /// </summary>
      [ Alias( 'type' ) ]
      property &Type : string
        read Ftype
        write Ftype;
      /// <summary>
      ///   Unique identifier for this result, 1-64 bytes
      /// </summary>
      [ Alias( 'id' ) ]
      property ID : string
        read FID
        write FID;
      /// <summary>
      ///   Optional. Inline keyboard attached to the message
      /// </summary>
      [ Alias( 'reply_markup' ) ]
      property Reply_markup : TtgInlineKeyboardMarkup
        read Freply_markup
        write Freply_markup;
      /// <summary>
      ///   Optional. Inline keyboard attached to the message
      /// </summary>
      [ Alias( 'input_message_content' ) ]
      property Input_message_content : TtgInputMessageContent
        read Finput_message_content
        write Finput_message_content;
  end;

  /// <summary>
  ///   Represents a link to an article or web page.
  /// </summary>
  [ Alias( 'InlineQueryResultArticle' ) ]
  TtgInlineQueryResultArticle = class( TtgInlineQueryResult )
    private
      Ftitle : string;
      Furl : string;
      Fhide_url : Boolean;
      Fdescription : string;
      Fthumb_url : string;
      Fthumb_width : Integer;
      Fthumb_height : Integer;
    public
      constructor Create;
    published
      /// <summary>
      ///   Title of the result
      /// </summary>
      [ Alias( 'title' ) ]
      property Title : string
        read Ftitle
        write Ftitle;
      /// <summary>
      ///   Optional. URL of the result
      /// </summary>
      [ Alias( 'url' ) ]
      property Url : string
        read Furl
        write Furl;
      /// <summary>
      ///   Optional. Pass True, if you don't want the URL to be shown in the
      ///   message
      /// </summary>
      [ Alias( 'hide_url' ) ]
      property Hide_url : Boolean
        read Fhide_url
        write Fhide_url;
      /// <summary>
      ///   Optional. Short description of the result
      /// </summary>
      [ Alias( 'description' ) ]
      property Description : string
        read Fdescription
        write Fdescription;
      /// <summary>
      ///   Optional. Url of the thumbnail for the result
      /// </summary>
      [ Alias( 'thumb_url' ) ]
      property Thumb_url : string
        read Fthumb_url
        write Fthumb_url;
      /// <summary>
      ///   Optional. Thumbnail width
      /// </summary>
      [ Alias( 'thumb_width' ) ]
      property Thumb_width : Integer
        read Fthumb_width
        write Fthumb_width;
      /// <summary>
      ///   Optional. Thumbnail height
      /// </summary>
      [ Alias( 'thumb_height' ) ]
      property Thumb_height : Integer
        read Fthumb_height
        write Fthumb_height;
  end;

  /// <summary>
  ///   Represents a link to a photo. By default, this photo will be sent by
  ///   the user with optional caption. Alternatively, you can use
  ///   input_message_content to send a message with the specified content
  ///   instead of the photo.
  /// </summary>
  [ Alias( 'InlineQueryResultPhoto' ) ]
  TtgInlineQueryResultPhoto = class( TtgInlineQueryResult )
    private
      Fphoto_url : string;
      Fthumb_url : string;
      Fphoto_width : Integer;
      Fphoto_height : Integer;
      Ftitle : string;
      Fdescription : string;
      FCaption : string;
    published
      /// <summary>
      ///   A valid URL of the photo. Photo must be in jpeg format. Photo size
      ///   must not exceed 5MB
      /// </summary>
      [ Alias( 'photo_url' ) ]
      property Photo_url : string
        read Fphoto_url
        write Fphoto_url;
      /// <summary>
      ///   URL of the thumbnail for the photo
      /// </summary>
      [ Alias( 'thumb_url' ) ]
      property Thumb_url : string
        read Fthumb_url
        write Fthumb_url;
      /// <summary>
      ///   Optional. Width of the photo
      /// </summary>
      [ Alias( 'photo_width' ) ]
      property Photo_width : Integer
        read Fphoto_width
        write Fphoto_width;
      /// <summary>
      ///   Optional. Height of the photo
      /// </summary>
      [ Alias( 'photo_height' ) ]
      property Photo_height : Integer
        read Fphoto_height
        write Fphoto_height;
      /// <summary>
      ///   Optional. Title for the result
      /// </summary>
      [ Alias( 'title' ) ]
      property Title : string
        read Ftitle
        write Ftitle;
      /// <summary>
      ///   Optional. Short description of the result
      /// </summary>
      [ Alias( 'description' ) ]
      property Description : string
        read Fdescription
        write Fdescription;
      /// <summary>
      ///   Optional. Caption of the photo to be sent, 0-200 characters
      /// </summary>
      [ Alias( 'caption' ) ]
      property Caption : string
        read FCaption
        write FCaption;
  end;

  /// <summary>
  ///   Represents a link to an animated GIF file. By default, this animated
  ///   GIF file will be sent by the user with optional caption. Alternatively,
  ///   you can use input_message_content to send a message with the specified
  ///   content instead of the animation.
  /// </summary>
  [ Alias( 'InlineQueryResultGif' ) ]
  TtgInlineQueryResultGif = class( TtgInlineQueryResult )
    private
      Fgif_url : string;
      Fgif_width : Integer;
      Fgif_height : Integer;
      Fthumb_url : string;
      Ftitle : string;
      FCaption : string;
      Fgif_duration : Integer;
    published
      /// <summary>
      ///   A valid URL for the GIF file. File size must not exceed 1MB
      /// </summary>
      [ Alias( 'gif_url' ) ]
      property Gif_url : string
        read Fgif_url
        write Fgif_url;
      /// <summary>
      ///   Optional. Width of the GIF
      /// </summary>
      [ Alias( 'gif_width' ) ]
      property Gif_width : Integer
        read Fgif_width
        write Fgif_width;
      /// <summary>
      ///   Optional. Height of the GIF
      /// </summary>
      [ Alias( 'gif_height' ) ]
      property Gif_height : Integer
        read Fgif_height
        write Fgif_height;

      /// <summary>
      ///   Optional. Duration of the GIF
      /// </summary>
      [ Alias( 'gif_duration' ) ]
      property Gif_duration : Integer
        read Fgif_duration
        write Fgif_duration;

      /// <summary>
      ///   URL of the static thumbnail for the result (jpeg or gif)
      /// </summary>
      [ Alias( 'thumb_url' ) ]
      property Thumb_url : string
        read Fthumb_url
        write Fthumb_url;
      /// <summary>
      ///   Optional. Title for the result
      /// </summary>
      [ Alias( 'title' ) ]
      property Title : string
        read Ftitle
        write Ftitle;
      /// <summary>
      ///   Optional. Caption of the GIF file to be sent, 0-200 characters
      /// </summary>
      [ Alias( 'caption' ) ]
      property Caption : string
        read FCaption
        write FCaption;
  end;

  [ Alias( 'InlineQueryResultMpeg4Gif' ) ]
  /// <summary>Represents a link to a video animation (H.264/MPEG-4 AVC video without sound). By default, this animated MPEG-4 file will be sent by the user with optional caption. Alternatively, you can use input_message_content to send a message with the specified content instead of the animation.</summary>
  TtgInlineQueryResultMpeg4Gif = class( TtgInlineQueryResult )
    private
      Fmpeg4_url : string;
      Fmpeg4_width : Integer;
      Fmpeg4_height : Integer;
      Fthumb_url : string;
      Ftitle : string;
      FCaption : string;
      Fmpeg4_duration : Integer;
    published
      /// <summary>
      ///   A valid URL for the MP4 file. File size must not exceed 1MB
      /// </summary>
      [ Alias( 'mpeg4_url' ) ]
      property Mpeg4_url : string
        read Fmpeg4_url
        write Fmpeg4_url;
      /// <summary>
      ///   Optional. Video width
      /// </summary>
      [ Alias( 'mpeg4_width' ) ]
      property Mpeg4_width : Integer
        read Fmpeg4_width
        write Fmpeg4_width;
      /// <summary>
      ///   Optional. Video height
      /// </summary>
      [ Alias( 'mpeg4_height' ) ]
      property Mpeg4_height : Integer
        read Fmpeg4_height
        write Fmpeg4_height;
      /// <summary>
      ///   Optional. Video height
      /// </summary>
      [ Alias( 'mpeg4_duration' ) ]
      property Mpeg4_duration : Integer
        read Fmpeg4_duration
        write Fmpeg4_duration;
      /// <summary>
      ///   URL of the static thumbnail (jpeg or gif) for the result
      /// </summary>
      [ Alias( 'thumb_url' ) ]
      property Thumb_url : string
        read Fthumb_url
        write Fthumb_url;
      /// <summary>
      ///   Optional. Title for the result
      /// </summary>
      [ Alias( 'title' ) ]
      property Title : string
        read Ftitle
        write Ftitle;
      /// <summary>
      ///   Optional. Caption of the MPEG-4 file to be sent, 0-200 characters
      /// </summary>
      [ Alias( 'caption' ) ]
      property Caption : string
        read FCaption
        write FCaption;

  end;

  /// <summary>
  ///   Represents a link to a page containing an embedded video player or a
  ///   video file. By default, this video file will be sent by the user with
  ///   an optional caption. Alternatively, you can use input_message_content
  ///   to send a message with the specified content instead of the video.
  /// </summary>
  [ Alias( 'InlineQueryResultVideo' ) ]
  TtgInlineQueryResultVideo = class( TtgInlineQueryResult )
    private
      Fvideo_url : string;
      Fmime_type : string;
      Fthumb_url : string;
      Ftitle : string;
      FCaption : string;
      Fvideo_width : Integer;
      Fvideo_height : Integer;
      Fvideo_duration : Integer;
      Fdescription : string;
    published
      /// <summary>
      ///   A valid URL for the embedded video player or video file
      /// </summary>
      [ Alias( 'video_url' ) ]
      property Video_url : string
        read Fvideo_url
        write Fvideo_url;
      /// <summary>
      ///   Mime type of the content of video url, “text/html” or “video/mp4”
      /// </summary>
      [ Alias( 'mime_type' ) ]
      property Mime_type : string
        read Fmime_type
        write Fmime_type;
      /// <summary>
      ///   URL of the thumbnail (jpeg only) for the video
      /// </summary>
      [ Alias( 'thumb_url' ) ]
      property Thumb_url : string
        read Fthumb_url
        write Fthumb_url;
      /// <summary>
      ///   Title for the result
      /// </summary>
      [ Alias( 'title' ) ]
      property Title : string
        read Ftitle
        write Ftitle;
      /// <summary>
      ///   Optional. Caption of the video to be sent, 0-200 characters
      /// </summary>
      [ Alias( 'caption' ) ]
      property Caption : string
        read FCaption
        write FCaption;
      /// <summary>
      ///   Optional. Video width
      /// </summary>
      [ Alias( 'video_width' ) ]
      property Video_width : Integer
        read Fvideo_width
        write Fvideo_width;
      /// <summary>
      ///   Optional. Video height
      /// </summary>
      [ Alias( 'video_height' ) ]
      property Video_height : Integer
        read Fvideo_height
        write Fvideo_height;
      /// <summary>
      ///   Optional. Video duration in seconds
      /// </summary>
      [ Alias( 'video_duration' ) ]
      property Video_duration : Integer
        read Fvideo_duration
        write Fvideo_duration;
      /// <summary>
      ///   Optional. Short description of the result
      /// </summary>
      [ Alias( 'description' ) ]
      property Description : string
        read Fdescription
        write Fdescription;
  end;

  /// <summary>
  ///   Represents a link to an mp3 audio file. By default, this audio file
  ///   will be sent by the user. Alternatively, you can use
  ///   input_message_content to send a message with the specified content
  ///   instead of the audio.
  /// </summary>
  [ Alias( 'InlineQueryResultAudio' ) ]
  TtgInlineQueryResultAudio = class( TtgInlineQueryResult )
    private
      Faudio_url : string;
      Ftitle : string;
      FPerformer : string;
      Faudio_duration : Integer;
    published
      /// <summary>
      ///   A valid URL for the audio file
      /// </summary>
      [ Alias( 'audio_url' ) ]
      property Audio_url : string
        read Faudio_url
        write Faudio_url;
      /// <summary>
      ///   Title
      /// </summary>
      [ Alias( 'title' ) ]
      property Title : string
        read Ftitle
        write Ftitle;
      /// <summary>
      ///   Optional. Performer
      /// </summary>
      [ Alias( 'performer' ) ]
      property Performer : string
        read FPerformer
        write FPerformer;
      /// <summary>
      ///   Optional. Audio duration in seconds
      /// </summary>
      [ Alias( 'audio_duration' ) ]
      property Audio_duration : Integer
        read Faudio_duration
        write Faudio_duration;
  end;

  /// <summary>
  ///   Represents a link to a voice recording in an .ogg container encoded
  ///   with OPUS. By default, this voice recording will be sent by the user.
  ///   Alternatively, you can use input_message_content to send a message with
  ///   the specified content instead of the the voice message.
  /// </summary>
  [ Alias( 'InlineQueryResultVoice' ) ]
  TtgInlineQueryResultVoice = class( TtgInlineQueryResult )
    private
      Fvoice_url : string;
      Ftitle : string;
      Fvoice_duration : Integer;
    published
      /// <summary>
      ///   A valid URL for the voice recording
      /// </summary>
      [ Alias( 'voice_url' ) ]
      property Voice_url : string
        read Fvoice_url
        write Fvoice_url;
      /// <summary>
      ///   Recording title
      /// </summary>
      [ Alias( 'title' ) ]
      property Title : string
        read Ftitle
        write Ftitle;
      /// <summary>
      ///   Optional. Recording duration in seconds
      /// </summary>
      [ Alias( 'voice_duration' ) ]
      property Voice_duration : Integer
        read Fvoice_duration
        write Fvoice_duration;
  end;

  /// <summary>
  ///   Represents a link to a file. By default, this file will be sent by the
  ///   user with an optional caption. Alternatively, you can use
  ///   input_message_content to send a message with the specified content
  ///   instead of the file. Currently, only .PDF and .ZIP files can be sent
  ///   using this method.
  /// </summary>
  [ Alias( 'InlineQueryResultDocument' ) ]
  TtgInlineQueryResultDocument = class( TtgInlineQueryResult )
    private
      Ftitle : string;
      FCaption : string;
      Fdocument_url : string;
      Fmime_type : string;
      Fdescription : string;
      Fthumb_url : string;
      Fthumb_width : Integer;
      Fthumb_height : Integer;
    published
      /// <summary>
      ///   Title for the result
      /// </summary>
      [ Alias( 'title' ) ]
      property Title : string
        read Ftitle
        write Ftitle;
      /// <summary>
      ///   Optional. Caption of the document to be sent, 0-200 characters
      /// </summary>
      [ Alias( 'caption' ) ]
      property Caption : string
        read FCaption
        write FCaption;
      /// <summary>
      ///   A valid URL for the file
      /// </summary>
      [ Alias( 'document_url' ) ]
      property Document_url : string
        read Fdocument_url
        write Fdocument_url;
      /// <summary>
      ///   Mime type of the content of the file, either “application/pdf” or
      ///   “application/zip”
      /// </summary>
      [ Alias( 'mime_type' ) ]
      property Mime_type : string
        read Fmime_type
        write Fmime_type;
      /// <summary>
      ///   Optional. Short description of the result
      /// </summary>
      [ Alias( 'description' ) ]
      property Description : string
        read Fdescription
        write Fdescription;
      /// <summary>
      ///   Optional. URL of the thumbnail (jpeg only) for the file
      /// </summary>
      [ Alias( 'thumb_url' ) ]
      property Thumb_url : string
        read Fthumb_url
        write Fthumb_url;
      /// <summary>
      ///   Optional. Thumbnail width
      /// </summary>
      [ Alias( 'thumb_width' ) ]
      property Thumb_width : Integer
        read Fthumb_width
        write Fthumb_width;
      /// <summary>
      ///   Optional. Thumbnail height
      /// </summary>
      [ Alias( 'thumb_height' ) ]
      property Thumb_height : Integer
        read Fthumb_height
        write Fthumb_height;
  end;

  /// <summary>
  ///   Represents a location on a map. By default, the location will be sent
  ///   by the user. Alternatively, you can use input_message_content to send a
  ///   message with the specified content instead of the location.
  /// </summary>
  [ Alias( 'InlineQueryResultLocation' ) ]
  TtgInlineQueryResultLocation = class( TtgInlineQueryResult )
    private
      FLatitude : Single;
      FLongitude : Single;
      Ftitle : string;
      Fthumb_url : string;
      Fthumb_width : Integer;
      Fthumb_height : Integer;
    published
      /// <summary>
      ///   Location latitude in degrees
      /// </summary>
      [ Alias( 'latitude' ) ]
      property Latitude : Single
        read FLatitude
        write FLatitude;
      /// <summary>
      ///   Location longitude in degrees
      /// </summary>
      [ Alias( 'longitude' ) ]
      property Longitude : Single
        read FLongitude
        write FLongitude;
      /// <summary>
      ///   Location title
      /// </summary>
      [ Alias( 'title' ) ]
      property Title : string
        read Ftitle
        write Ftitle;
      /// <summary>
      ///   Optional. Url of the thumbnail for the result
      /// </summary>
      [ Alias( 'thumb_url' ) ]
      property Thumb_url : string
        read Fthumb_url
        write Fthumb_url;
      /// <summary>
      ///   Optional. Thumbnail width
      /// </summary>
      [ Alias( 'thumb_width' ) ]
      property Thumb_width : Integer
        read Fthumb_width
        write Fthumb_width;
      /// <summary>
      ///   Optional. Thumbnail height
      /// </summary>
      [ Alias( 'thumb_height' ) ]
      property Thumb_height : Integer
        read Fthumb_height
        write Fthumb_height;
  end;

  /// <summary>
  ///   Represents a venue. By default, the venue will be sent by the user.
  ///   Alternatively, you can use input_message_content to send a message with
  ///   the specified content instead of the venue.
  /// </summary>
  [ Alias( 'InlineQueryResultVenue' ) ]
  TtgInlineQueryResultVenue = class( TtgInlineQueryResult )
    private
      FLatitude : Single;
      Ftitle : string;
      FLongitude : Single;
      Fthumb_width : Integer;
      Fthumb_url : string;
      Fthumb_height : Integer;
      FAddress : string;
      Ffoursquare_id : string;
    published
      /// <summary>
      ///   Latitude of the venue location in degrees
      /// </summary>
      [ Alias( 'latitude' ) ]
      property Latitude : Single
        read FLatitude
        write FLatitude;
      /// <summary>
      ///   Longitude of the venue location in degrees
      /// </summary>
      [ Alias( 'longitude' ) ]
      property Longitude : Single
        read FLongitude
        write FLongitude;
      /// <summary>
      ///   Title of the venue
      /// </summary>
      [ Alias( 'title' ) ]
      property Title : string
        read Ftitle
        write Ftitle;
      /// <summary>
      ///   Address of the venue
      /// </summary>
      [ Alias( 'address' ) ]
      property Address : string
        read FAddress
        write FAddress;
      /// <summary>
      ///   Optional. Foursquare identifier of the venue if known
      /// </summary>
      [ Alias( 'foursquare_id' ) ]
      property Foursquare_id : string
        read Ffoursquare_id
        write Ffoursquare_id;
      /// <summary>
      ///   Optional. Url of the thumbnail for the result
      /// </summary>
      [ Alias( 'thumb_url' ) ]
      property Thumb_url : string
        read Fthumb_url
        write Fthumb_url;
      /// <summary>
      ///   Optional. Thumbnail width
      /// </summary>
      [ Alias( 'thumb_width' ) ]
      property Thumb_width : Integer
        read Fthumb_width
        write Fthumb_width;
      /// <summary>
      ///   Optional. Thumbnail height
      /// </summary>
      [ Alias( 'thumb_height' ) ]
      property Thumb_height : Integer
        read Fthumb_height
        write Fthumb_height;
  end;

  /// <summary>
  ///   Represents a contact with a phone number. By default, this contact will
  ///   be sent by the user. Alternatively, you can use input_message_content
  ///   to send a message with the specified content instead of the contact.
  /// </summary>
  [ Alias( 'InlineQueryResultContact' ) ]
  TtgInlineQueryResultContact = class
    public
      /// <summary>
      ///   Contact's phone number
      /// </summary>
      [ Alias( 'phone_number' ) ]
      Phone_number : string;
      /// <summary>
      ///   Contact's first name
      /// </summary>
      [ Alias( 'first_name' ) ]
      First_name : string;
      /// <summary>
      ///   Optional. Contact's last name
      /// </summary>
      [ Alias( 'last_name' ) ]
      Last_name : string;
      /// <summary>
      ///   Optional. Url of the thumbnail for the result
      /// </summary>
      [ Alias( 'thumb_url' ) ]
      Thumb_url : string;
      /// <summary>
      ///   Optional. Thumbnail width
      /// </summary>
      [ Alias( 'thumb_width' ) ]
      Thumb_width : Integer;
      /// <summary>
      ///   Optional. Thumbnail height
      /// </summary>
      [ Alias( 'thumb_height' ) ]
      Thumb_height : Integer;
  end;

  TtgInlineQueryResultCached = class( TtgInlineQueryResult )
  end;

  /// <summary>
  ///   Represents a link to a photo stored on the Telegram servers. By
  ///   default, this photo will be sent by the user with an optional caption.
  ///   Alternatively, you can use input_message_content to send a message with
  ///   the specified content instead of the photo.
  /// </summary>
  [ Alias( 'InlineQueryResultCachedPhoto' ) ]
  TtgInlineQueryResultCachedPhoto = class( TtgInlineQueryResultCached )
    public
      /// <summary>
      ///   A valid file identifier of the photo
      /// </summary>
      [ Alias( 'photo_file_id' ) ]
      Photo_file_id : string;
      /// <summary>
      ///   Optional. Title for the result
      /// </summary>
      [ Alias( 'title' ) ]
      Title : string;
      /// <summary>
      ///   Optional. Short description of the result
      /// </summary>
      [ Alias( 'description' ) ]
      Description : string;
      /// <summary>
      ///   Optional. Caption of the photo to be sent, 0-200 characters
      /// </summary>
      [ Alias( 'caption' ) ]
      Caption : string;
  end;

  /// <summary>
  ///   Represents a link to an animated GIF file stored on the Telegram
  ///   servers. By default, this animated GIF file will be sent by the user
  ///   with an optional caption. Alternatively, you can use
  ///   input_message_content to send a message with specified content instead
  ///   of the animation.
  /// </summary>
  [ Alias( 'InlineQueryResultCachedGif' ) ]
  TtgInlineQueryResultCachedGif = class( TtgInlineQueryResultCached )
    public
      /// <summary>
      ///   A valid file identifier for the GIF file
      /// </summary>
      [ Alias( 'gif_file_id' ) ]
      Gif_file_id : string;
      /// <summary>
      ///   Optional. Title for the result
      /// </summary>
      [ Alias( 'title' ) ]
      Title : string;
      /// <summary>
      ///   Optional. Caption of the GIF file to be sent, 0-200 characters
      /// </summary>
      [ Alias( 'caption' ) ]
      Caption : string;
  end;

  /// <summary>
  ///   Represents a link to a video animation (H.264/MPEG-4 AVC video without
  ///   sound) stored on the Telegram servers. By default, this animated MPEG-4
  ///   file will be sent by the user with an optional caption. Alternatively,
  ///   you can use input_message_content to send a message with the specified
  ///   content instead of the animation.
  /// </summary>
  [ Alias( 'InlineQueryResultCachedMpeg4Gif' ) ]
  TtgInlineQueryResultCachedMpeg4Gif = class( TtgInlineQueryResultCached )
    public
      /// <summary>
      ///   A valid file identifier for the MP4 file
      /// </summary>
      [ Alias( 'mpeg4_file_id' ) ]
      Mpeg4_file_id : string;
      /// <summary>
      ///   Optional. Title for the result
      /// </summary>
      [ Alias( 'title' ) ]
      Title : string;
      /// <summary>
      ///   Optional. Caption of the MPEG-4 file to be sent, 0-200 characters
      /// </summary>
      [ Alias( 'caption' ) ]
      Caption : string;
  end;

  /// <summary>
  ///   Represents a link to a sticker stored on the Telegram servers. By
  ///   default, this sticker will be sent by the user. Alternatively, you can
  ///   use input_message_content to send a message with the specified content
  ///   instead of the sticker.
  /// </summary>
  [ Alias( 'InlineQueryResultCachedSticker' ) ]
  TtgInlineQueryResultCachedSticker = class( TtgInlineQueryResultCached )
    public
      /// <summary>
      ///   A valid file identifier of the sticker
      /// </summary>
      [ Alias( 'sticker_file_id' ) ]
      Sticker_file_id : string;
  end;

  /// <summary>
  ///   Represents a link to a file stored on the Telegram servers. By default,
  ///   this file will be sent by the user with an optional caption.
  ///   Alternatively, you can use input_message_content to send a message with
  ///   the specified content instead of the file. Currently, only pdf-files
  ///   and zip archives can be sent using this method.
  /// </summary>
  [ Alias( 'InlineQueryResultCachedDocument' ) ]
  TTgInlineQueryResultCachedDocument = class( TtgInlineQueryResultCached )
    public
      /// <summary>
      ///   Title for the result
      /// </summary>
      [ Alias( 'title' ) ]
      Title : string;
      /// <summary>
      ///   A valid file identifier for the file
      /// </summary>
      [ Alias( 'document_file_id' ) ]
      Document_file_id : string;
      /// <summary>
      ///   Optional. Short description of the result
      /// </summary>
      [ Alias( 'description' ) ]
      Description : string;
      /// <summary>
      ///   Optional. Caption of the document to be sent, 0-200 characters
      /// </summary>
      [ Alias( 'caption' ) ]
      Caption : string;
  end;

  /// <summary>
  ///   Represents a link to a video file stored on the Telegram servers. By
  ///   default, this video file will be sent by the user with an optional
  ///   caption. Alternatively, you can use input_message_content to send a
  ///   message with the specified content instead of the video.
  /// </summary>
  [ Alias( 'InlineQueryResultCachedVideo' ) ]
  TtgInlineQueryResultCachedVideo = class( TtgInlineQueryResultCached )
    public
      /// <summary>
      ///   Title for the result
      /// </summary>
      [ Alias( 'title' ) ]
      Title : string;
      /// <summary>
      ///   A valid file identifier for the video
      /// </summary>
      [ Alias( 'video_file_id' ) ]
      Video_file_id : string;
      /// <summary>
      ///   Optional. Short description of the result
      /// </summary>
      [ Alias( 'description' ) ]
      Description : string;
      /// <summary>
      ///   Optional. Caption of the video to be sent, 0-200 characters
      /// </summary>
      [ Alias( 'caption' ) ]
      Caption : string;
  end;

  /// <summary>
  ///   Represents a link to a voice message stored on the Telegram servers. By
  ///   default, this voice message will be sent by the user. Alternatively,
  ///   you can use input_message_content to send a message with the specified
  ///   content instead of the voice message.
  /// </summary>
  [ Alias( 'InlineQueryResultCachedVoice' ) ]
  TtgInlineQueryResultCachedVoice = class( TtgInlineQueryResultCached )
    public
      /// <summary>
      ///   A valid file identifier for the voice message
      /// </summary>
      [ Alias( 'voice_file_id' ) ]
      Voice_file_id : string;
      /// <summary>
      ///   Voice message title
      /// </summary>
      [ Alias( 'title' ) ]
      Title : string;
  end;

  /// <summary>
  ///   Represents a link to an mp3 audio file stored on the Telegram servers.
  ///   By default, this audio file will be sent by the user. Alternatively,
  ///   you can use input_message_content to send a message with the specified
  ///   content instead of the audio.
  /// </summary>
  [ Alias( 'InlineQueryResultCachedAudio' ) ]
  TtgInlineQueryResultCachedAudio = class( TtgInlineQueryResultCached )
    public
      /// <summary>
      ///   A valid file identifier for the audio file
      /// </summary>
      [ Alias( 'audio_file_id' ) ]
      Audio_file_id : string;
  end;

  /// <summary>
  ///   Contains information about the current status of a webhook.
  /// </summary>
  TtgWebhookInfo = class
    public
      /// <summary>
      ///   Webhook URL, may be empty if webhook is not set up
      /// </summary>
      [ Alias( 'url' ) ]
      Url : string;
      /// <summary>
      ///   True, if a custom certificate was provided for webhook certificate
      ///   checks
      /// </summary>
      [ Alias( 'has_custom_certificate' ) ]
      Has_custom_certificate : Boolean;
      /// <summary>
      ///   Number of updates awaiting delivery
      /// </summary>
      [ Alias( 'pending_update_count' ) ]
      Pending_update_count : Integer;
      /// <summary>
      ///   Optional. Unix time for the most recent error that happened when
      ///   trying to deliver an update via webhook
      /// </summary>
      [ Alias( 'last_error_date' ) ]
      Last_error_date : Integer;
      /// <summary>
      ///   Optional. Error message in human-readable format for the most
      ///   recent error that happened when trying to deliver an update via
      ///   webhook
      /// </summary>
      [ Alias( 'last_error_message' ) ]
      Last_error_message : string;
      /// <summary>
      ///   Optional. Maximum allowed number of simultaneous HTTPS connections
      ///   to the webhook for update delivery
      /// </summary>
      [ Alias( 'max_connections' ) ]
      Max_connections : Integer;
      /// <summary>
      ///   Optional. A list of update types the bot is subscribed to. Defaults
      ///   to all update types
      /// </summary>
      [ Alias( 'allowed_updates' ) ]
      Allowed_updates : TArray< string >;
  end;

{$REGION 'Payments'}

  /// <summary>
  ///   This object represents a portion of the price for goods or services.
  /// </summary>
  [ Alias( 'LabeledPrice' ) ]
  TtgLabeledPrice = class
    public
      /// <summary>
      ///   Portion label
      /// </summary>
      [ Alias( 'label' ) ]
      &label : string;
      /// <summary>
      ///   Price of the product in the smallest units of the <see href="https://core.telegram.org/bots/payments#supported-currencies">
      ///   currency</see> (integer, not float/double).
      /// </summary>
      /// <example>
      ///   For example, for a price of <c>US$ 1.45</c> pass <c>amount = 145</c>
      ///   . See the exp parameter in <see href="https://core.telegram.org/bots/payments/currencies.json">
      ///   currencies.json</see>, it shows the number of digits past the
      ///   decimal point for each <br />currency (2 for the majority of
      ///   currencies). <br />
      /// </example>
      Amount : Integer;
  end;

  /// <summary>
  ///   This object contains basic information about an invoice.
  /// </summary>
  TtgInvoice = class
    public
      /// <summary>
      ///   Product name
      /// </summary>
      [ Alias( 'title' ) ]
      Title : string;
      /// <summary>
      ///   Product description
      /// </summary>
      [ Alias( 'description' ) ]
      Description : string;
      /// <summary>
      ///   Unique bot deep-linking parameter that can be used to generate this
      ///   invoice
      /// </summary>
      [ Alias( 'start_parameter' ) ]
      Start_parameter : string;
      /// <summary>
      ///   Three-letter ISO 4217 <see href="https://core.telegram.org/bots/payments#supported-currencies">
      ///   currency</see> code
      /// </summary>
      [ Alias( 'currency' ) ]
      Currency : string;
      /// <summary>
      ///   Total price in the smallest units of the currency (integer, not
      ///   float/double). For example, for a price of <c>US$ 1.45</c> pass <c>
      ///   amount = 145</c>. See the <c>exp</c> parameter in <see href="https://core.telegram.org/bots/payments/currencies.json">
      ///   currencies.json</see>, it shows the number of digits past the
      ///   decimal point for each currency (2 for the majority of currencies).
      /// </summary>
      [ Alias( 'total_amount' ) ]
      Total_amount : Integer;
  end;

  /// <summary>
  ///   This object represents a shipping address.
  /// </summary>
  TtgShippingAddress = class
    public
      /// <summary>
      ///   ISO 3166-1 alpha-2 country code
      /// </summary>
      [ Alias( 'country_code' ) ]
      Country_code : string;
      /// <summary>
      ///   State, if applicable
      /// </summary>
      [ Alias( 'state' ) ]
      State : string;
      /// <summary>
      ///   City
      /// </summary>
      [ Alias( 'city' ) ]
      City : string;
      /// <summary>
      ///   First line for the address
      /// </summary>
      [ Alias( 'street_line1' ) ]
      Street_line1 : string;
      /// <summary>
      ///   Second line for the address
      /// </summary>
      [ Alias( 'street_line2' ) ]
      Street_line2 : string;
      /// <summary>
      ///   Address post code
      /// </summary>
      [ Alias( 'post_code' ) ]
      Post_code : string;
  end;

  /// <summary>
  ///   This object represents information about an order.
  /// </summary>
  TtgOrderInfo = class
    public
      /// <summary>
      ///   Optional. User name
      /// </summary>
      [ Alias( 'name' ) ]
      Name : string;
      /// <summary>
      ///   Optional. User's phone number
      /// </summary>
      [ Alias( 'phone_number' ) ]
      Phone_number : string;
      /// <summary>
      ///   Optional. User email
      /// </summary>
      [ Alias( 'email' ) ]
      Email : string;
      /// <summary>
      ///   Optional. User shipping address
      /// </summary>
      [ Alias( 'shipping_address' ) ]
      Shipping_address : TtgShippingAddress;
  end;

  /// <summary>
  ///   This object represents one shipping option.
  /// </summary>
  TtgShippingOption = class
    public
      /// <summary>
      ///   Shipping option identifier
      /// </summary>
      [ Alias( 'id' ) ]
      ID : string;
      /// <summary>
      ///   Option title
      /// </summary>
      [ Alias( 'title' ) ]
      Title : string;
      /// <summary>
      ///   List of price portions
      /// </summary>
      [ Alias( 'prices' ) ]
      Prices : TArray< TtgLabeledPrice >;
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
      [ Alias( 'currency' ) ]
      Currency : string;
      /// <summary>
      ///   Total price in the smallest units of the currency (integer, not
      ///   float/double). For example, for a price of <c>US$ 1.45</c> pass <c>
      ///   amount = 145</c>. See the <c>exp</c> parameter in <see href="https://core.telegram.org/bots/payments/currencies.json">
      ///   currencies.json</see>, it shows the number of digits past the
      ///   decimal point for each currency (2 for the majority of currencies).
      /// </summary>
      [ Alias( 'total_amount' ) ]
      Total_amount : Integer;
      /// <summary>
      ///   Bot specified invoice payload
      /// </summary>
      [ Alias( 'invoice_payload' ) ]
      Invoice_payload : string;
      /// <summary>
      ///   Optional. Identifier of the shipping option chosen by the user
      /// </summary>
      [ Alias( 'shipping_option_id' ) ]
      Shipping_option_id : string;
      /// <summary>
      ///   Optional. Order info provided by the user
      /// </summary>
      [ Alias( 'order_info' ) ]
      Order_info : TtgOrderInfo;
      /// <summary>
      ///   Telegram payment identifier
      /// </summary>
      [ Alias( 'telegram_payment_charge_id' ) ]
      Telegram_payment_charge_id : string;
      /// <summary>
      ///   Provider payment identifier
      /// </summary>
      [ Alias( 'provider_payment_charge_id' ) ]
      Provider_payment_charge_id : string;
  end;

  /// <summary>
  ///   This object contains information about an incoming shipping query.
  /// </summary>
  TtgShippingQuery = class
    public
      /// <summary>
      ///   Unique query identifier
      /// </summary>
      [ Alias( 'id' ) ]
      ID : string;
      /// <summary>
      ///   User who sent the query
      /// </summary>
      [ Alias( 'from' ) ]
      From : TtgUser;
      /// <summary>
      ///   Bot specified invoice payload
      /// </summary>
      [ Alias( 'invoice_payload' ) ]
      Invoice_payload : string;
      /// <summary>
      ///   User specified shipping address
      /// </summary>
      [ Alias( 'shipping_address' ) ]
      Shipping_address : TtgShippingAddress;
  end;

  /// <summary>
  ///   This object contains information about an incoming pre-checkout query.
  /// </summary>
  [ Alias( 'PreCheckoutQuery' ) ]
  TtgPreCheckoutQuery = class
    public
      /// <summary>
      ///   Unique query identifier
      /// </summary>
      [ Alias( 'id' ) ]
      ID : string;
      /// <summary>
      ///   User who sent the query
      /// </summary>
      [ Alias( 'from' ) ]
      From : TtgUser;
      /// <summary>
      ///   Three-letter ISO 4217 <see href="https://core.telegram.org/bots/payments#supported-currencies">
      ///   currency</see> code
      /// </summary>
      [ Alias( 'currency' ) ]
      Currency : string;
      /// <summary>
      ///   Total price in the smallest units of the currency (integer, not
      ///   float/double). For example, for a price of <c>US$ 1.45</c> pass <c>
      ///   amount = 145</c>. See the <c>exp</c> parameter in <see href="https://core.telegram.org/bots/payments/currencies.json">
      ///   currencies.json</see>, it shows the number of digits past the
      ///   decimal point for each currency (2 for the majority of currencies).
      /// </summary>
      [ Alias( 'total_amount' ) ]
      Total_amount : Integer;
      /// <summary>
      ///   Bot specified invoice payload
      /// </summary>
      [ Alias( 'invoice_payload' ) ]
      Invoice_payload : string;
      /// <summary>
      ///   Optional. Identifier of the shipping option chosen by the user
      /// </summary>
      [ Alias( 'shipping_option_id' ) ]
      Shipping_option_id : string;
      /// <summary>
      ///   Optional. Order info provided by the user
      /// </summary>
      [ Alias( 'order_info' ) ]
      Order_info : TtgOrderInfo;
  end;
{$ENDREGION}

implementation

{ TtgApiFileToSend }

constructor TtgFileToSend.Create(
  const FileName : string;
  const Content  : TStream );
  begin
    Self.FileName := FileName;
    Self.Content := Content;
  end;

destructor TtgFileToSend.Destroy;
  begin
    Content.Free;
    inherited;
  end;

{ TtgKeyboardButton }

constructor TtgKeyboardButton.Create(
  const Text                        : string;
  Request_contact, Request_location : Boolean );
  begin
    Self.Text := Text;
    Self.Request_contact := Request_contact;
    Self.Request_location := Request_location;
  end;

{ TtgChatMember }

destructor TtgChatMember.Destroy;
  begin
    if Assigned( User )
    then
      FreeAndNil( User );
    inherited;
  end;

{ TtgMessageEntity }

destructor TtgMessageEntity.Destroy;
  begin
    if Assigned( User )
    then
      FreeAndNil( User );
    inherited;
  end;

{ TtgDocument }

destructor TtgDocument.Destroy;
  begin
    FreeAndNil( Thumb );
    inherited;
  end;

{ TtgSticker }

destructor TtgSticker.Destroy;
  begin
    FreeAndNil( Thumb );
    inherited;
  end;

{ TtgVideo }

destructor TtgVideo.Destroy;
  begin
    if Assigned( Thumb )
    then
      FreeAndNil( Thumb );
    inherited;
  end;

{ TtgVenue }

destructor TtgVenue.Destroy;
  begin
    if Assigned( Location )
    then
      FreeAndNil( Location );
    inherited;
  end;

{ TtgMessage }

destructor TtgMessage.Destroy;
  var
    I : Integer;
  begin
    if Assigned( From )
    then
      FreeAndNil( From );
    if Assigned( Chat )
    then
      FreeAndNil( Chat );
    if Assigned( ForwardFrom )
    then
      FreeAndNil( ForwardFrom );
    if Assigned( ForwardFromChat )
    then
      FreeAndNil( ForwardFromChat );
    if Assigned( ReplyToMessage )
    then
      FreeAndNil( ReplyToMessage );
    if Assigned( Audio )
    then
      FreeAndNil( Audio );
    if Assigned( Document )
    then
      FreeAndNil( Document );
    if Assigned( Sticker )
    then
      FreeAndNil( Sticker );
    if Assigned( Video )
    then
      FreeAndNil( Video );
    if Assigned( Voice )
    then
      FreeAndNil( Voice );
    if Assigned( Contact )
    then
      FreeAndNil( Contact );
    if Assigned( Location )
    then
      FreeAndNil( Location );
    if Assigned( Venue )
    then
      FreeAndNil( Venue );

    if Assigned( LeftChatMember )
    then
      FreeAndNil( LeftChatMember );
    if Assigned( PinnedMessage )
    then
      FreeAndNil( PinnedMessage );
    if Assigned( ForwardFromChat )
    then
      FreeAndNil( ForwardFromChat );
    if Assigned( NewChatMembers )
    then
    begin
      if Length( NewChatMembers ) > 0
      then
        for I := low( NewChatMembers ) to high( NewChatMembers ) do
          FreeAndNil( NewChatMembers[ I ] );
    end;
    if Assigned( Entities )
    then
    begin
      if Length( Entities ) > 0
      then
        for I := low( Entities ) to high( Entities ) do
          Entities[ I ].Free;
    end;
    for I := low( NewChatPhoto ) to high( NewChatPhoto ) do
      FreeAndNil( NewChatPhoto[ I ] );
    SetLength( NewChatPhoto, 0 );
    if Assigned( Game )
    then
      FreeAndNil( Game );
    inherited;
  end;

{ TtgUpdate }

destructor TtgUpdate.Destroy;
  begin
    if Assigned( message )
    then
      FreeAndNil( message );
    if Assigned( EditedMessage )
    then
      FreeAndNil( EditedMessage );
    if Assigned( Channel_post )
    then
      FreeAndNil( Channel_post );
    if Assigned( Edited_channel_post )
    then
      FreeAndNil( Edited_channel_post );
    if Assigned( InlineQuery )
    then
      FreeAndNil( InlineQuery );
    if Assigned( ChosenInlineResult )
    then
      FreeAndNil( ChosenInlineResult );
    if Assigned( CallbackQuery )
    then
      FreeAndNil( CallbackQuery );
    inherited;
  end;

{ TtgInlineQueryResult }

destructor TtgInlineQueryResult.Destroy;
  begin
    FreeAndNil( Freply_markup );
    FreeAndNil( Finput_message_content );
    inherited;
  end;

{ TtgAnimation }

destructor TtgAnimation.Destroy;
  begin
    if Assigned( Thumb )
    then
      FreeAndNil( Thumb );
    inherited;
  end;

{ TtgGame }

destructor TtgGame.Destroy;
  var
    I : Integer;
  begin
    for I := low( Photo ) to high( Photo ) do
      if Assigned( Photo[ I ] )
      then
        FreeAndNil( Photo[ I ] );
    for I := low( Text_entities ) to high( Text_entities ) do
      if Assigned( Text_entities[ I ] )
      then
        FreeAndNil( Text_entities[ I ] );
    if Assigned( Animation )
    then
      FreeAndNil( Animation );
    inherited;
  end;

{ TtgGameHighScore }

destructor TtgGameHighScore.Destroy;
  begin
    if Assigned( User )
    then
      FreeAndNil( User );
    inherited;
  end;

{ TtgReplyKeyboardMarkup }

destructor TtgReplyKeyboardMarkup.Destroy;
  var
    I, J : Integer;
  begin
    for I := low( KeyBoard ) to high( KeyBoard ) do
      for J := low( KeyBoard[ I ] ) to high( KeyBoard[ I ] ) do
        KeyBoard[ I, J ].Free;
    inherited;
  end;

function TtgReplyKeyboardMarkup.GetColCount : Integer;
  begin
    Result := 0;
    if RowCount > 0
    then
      Result := Length( KeyBoard[ 0 ] );
  end;

function TtgReplyKeyboardMarkup.GetRowCount : Integer;
  begin
    Result := Length( KeyBoard );
  end;

procedure TtgReplyKeyboardMarkup.SetColCount( const Value : Integer );
  var
    I : Integer;
  begin
    for I := low( KeyBoard ) to high( KeyBoard ) do
      SetLength( KeyBoard[ I ], Value );
  end;

procedure TtgReplyKeyboardMarkup.SetRowCount( const Value : Integer );
  begin
    SetLength( KeyBoard, Value );
  end;

{ TtgInlineQuery }

destructor TtgInlineQuery.Destroy;
  begin
    if Assigned( FFrom )
    then
      FreeAndNil( FFrom );
    inherited;
  end;

{ TtgInlineQueryResultArticle }

constructor TtgInlineQueryResultArticle.Create;
  begin
    inherited;
    Ftype := 'article';
  end;

{ TtgUserProfilePhotos }

destructor TtgUserProfilePhotos.Destroy;
  var
    I : Integer;
    J : Integer;
  begin
    for I := low( Photos ) to high( Photos ) do
      for J := low( Photos[ I ] ) to high( Photos[ I ] ) do
        FreeAndNil( Photos[ I, J ] );
    inherited;
  end;

{ TtgFile }

function TtgFile.GetFileUrl( const AToken : string ) : string;
  begin
    Result := Format( 'https://api.telegram.org/file/bot%S/%S',
      [ AToken, FilePath ] )
  end;

end.
