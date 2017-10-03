unit TelegaPi.Types.InlineQueryResults;

interface

uses
  DJSON.Attributes,
  TelegaPi.Types.ReplyMarkups,
  TelegaPi.Types.InputMessageContents;

type

  /// <summary>
  ///   This object represents one result of an inline query. Telegram clients
  ///   currently support results of the following 19 types ///
  /// </summary>
  [djName('InlineQueryResult')]
  TtgInlineQueryResult = class abstract
  public
    /// <summary>
    ///   Unique identifier for this result, 1-64 bytes
    /// </summary>
    [djName('id')]
    ID: string;
    /// <summary>
    ///   Type of the result
    /// </summary>
    [djName('type')]
    &Type: string;
    /// <summary>
    ///   Title of the result
    /// </summary>
    [djName('title')]
    Title: String;
    /// <summary>
    ///   Optional. Inline keyboard attached to the message
    /// </summary>
    [djName('input_message_content')]
    InputMessageContent: TtgInputMessageContent;
    /// <summary>
    ///   Optional. Inline keyboard attached to the message
    /// </summary>
    [djName('reply_markup')]
    ReplyMarkup: TtgInlineKeyboardMarkup;
    constructor Create;
    destructor Destroy; override;
  end;

  TtgInlineQueryResultNew = class(TtgInlineQueryResult)
  public
    /// <summary>
    ///   Optional. Url of the thumbnail for the result
    /// </summary>
    [djName('thumb_url')]
    ThumbUrl: string;
    /// <summary>
    ///   Optional. Thumbnail width
    /// </summary>
    [djName('thumb_width')]
    ThumbWidth: Integer;
    /// <summary>
    ///   Optional. Thumbnail height
    /// </summary>
    [djName('thumb_height')]
    ThumbHeight: Integer;
  end;

  /// <summary>
  ///   Represents a link to an article or web page.
  /// </summary>
  [djName('InlineQueryResultArticle')]
  TtgInlineQueryResultArticle = class(TtgInlineQueryResultNew)
  public
    /// <summary>
    ///   Optional. URL of the result
    /// </summary>
    [djName('url')]
    Url: string;
    /// <summary>
    ///   Optional. Pass True, if you don't want the URL to be shown in the
    ///   message
    /// </summary>
    [djName('hide_url')]
    HideUrl: Boolean;
    /// <summary>
    ///   Optional. Short description of the result
    /// </summary>
    [djName('description')]
    Description: string;
    constructor Create;
  end;

  /// <summary>
  ///   Represents a link to an mp3 audio file. By default, this audio file
  ///   will be sent by the user. Alternatively, you can use
  ///   input_message_content to send a message with the specified content
  ///   instead of the audio.
  /// </summary>
  [djName('InlineQueryResultAudio')]
  TtgInlineQueryResultAudio = class(TtgInlineQueryResultNew)
  public
    /// <summary>
    ///   A valid file identifier for the audio file
    /// </summary>
    [djName('audio_file_id')]
    FileId: string;
    /// <summary>
    ///   A valid URL for the audio file
    /// </summary>
    [djName('audio_url')]
    Url: string;
    /// <summary>
    ///   Optional. Performer
    /// </summary>
    [djName('performer')]
    Performer: string;
    /// <summary>
    ///   Optional. Audio duration in seconds
    /// </summary>
    [djName('audio_duration')]
    Duration: Integer;
    constructor Create;
  end;

  /// <summary>
  ///   Represents a link to a result stored on the Telegram servers. By
  ///   default, this result will be sent by the user with an optional caption.
  ///   Alternatively, you can use input_message_content to send a message with
  ///   the specified content instead of the photo.
  /// </summary>
  TtgInlineQueryResultCached = class(TtgInlineQueryResult)
    /// <summary>
    ///   Optional. Caption of the result to be sent, 0-200 characters
    /// </summary>
    [djName('caption')]
    Caption: string;
  end;

  /// <summary>
  ///   Represents a link to an mp3 audio file stored on the Telegram servers.
  ///   By default, this audio file will be sent by the user. Alternatively,
  ///   you can use input_message_content to send a message with the specified
  ///   content instead of the audio.
  /// </summary>
  [djName('InlineQueryResultCachedAudio')]
  TtgInlineQueryResultCachedAudio = class(TtgInlineQueryResultCached)
  public
    /// <summary>
    ///   A valid file identifier for the audio file
    /// </summary>
    [djName('audio_file_id')]
    FileId: string;
    constructor Create;
  end;

  /// <summary>
  ///   Represents a link to a file stored on the Telegram servers. By default,
  ///   this file will be sent by the user with an optional caption.
  ///   Alternatively, you can use input_message_content to send a message with
  ///   the specified content instead of the file. Currently, only pdf-files
  ///   and zip archives can be sent using this method.
  /// </summary>
  [djName('InlineQueryResultCachedDocument')]
  TTgInlineQueryResultCachedDocument = class(TtgInlineQueryResultCached)
  public
    /// <summary>
    ///   A valid file identifier for the file
    /// </summary>
    [djName('document_file_id')]
    FileId: string;
    /// <summary>
    ///   Optional. Short description of the result
    /// </summary>
    [djName('description')]
    Description: string;
    constructor Create;
  end;

  /// <summary>
  ///   Represents a link to an animated GIF file stored on the Telegram
  ///   servers. By default, this animated GIF file will be sent by the user
  ///   with an optional caption. Alternatively, you can use
  ///   input_message_content to send a message with specified content instead
  ///   of the animation.
  /// </summary>
  [djName('InlineQueryResultCachedGif')]
  TtgInlineQueryResultCachedGif = class(TtgInlineQueryResultCached)
  public
    /// <summary>
    ///   A valid file identifier for the GIF file
    /// </summary>
    [djName('gif_file_id')]
    FileId: string;
    constructor Create;
  end;

  /// <summary>
  ///   Represents a link to a video animation (H.264/MPEG-4 AVC video without
  ///   sound) stored on the Telegram servers. By default, this animated MPEG-4
  ///   file will be sent by the user with an optional caption. Alternatively,
  ///   you can use input_message_content to send a message with the specified
  ///   content instead of the animation.
  /// </summary>
  [djName('InlineQueryResultCachedMpeg4Gif')]
  TtgInlineQueryResultCachedMpeg4Gif = class(TtgInlineQueryResultCached)
  public
    /// <summary>
    ///   A valid file identifier for the MP4 file
    /// </summary>
    [djName('mpeg4_file_id')]
    FileId: string;
    constructor Create;
  end;

  /// <summary>
  ///   Represents a link to a photo stored on the Telegram servers. By
  ///   default, this photo will be sent by the user with an optional caption.
  ///   Alternatively, you can use input_message_content to send a message with
  ///   the specified content instead of the photo.
  /// </summary>
  [djName('InlineQueryResultCachedPhoto')]
  TtgInlineQueryResultCachedPhoto = class(TtgInlineQueryResultCached)
  public
    /// <summary>
    ///   A valid file identifier of the photo
    /// </summary>
    [djName('photo_file_id')]
    FileId: string;
    /// <summary>
    ///   Optional. Short description of the result
    /// </summary>
    [djName('description')]
    Description: string;
    constructor Create;
  end;

  /// <summary>
  ///   Represents a link to a sticker stored on the Telegram servers. By
  ///   default, this sticker will be sent by the user. Alternatively, you can
  ///   use input_message_content to send a message with the specified content
  ///   instead of the sticker.
  /// </summary>
  [djName('InlineQueryResultCachedSticker')]
  TtgInlineQueryResultCachedSticker = class(TtgInlineQueryResultCached)
  public
    /// <summary>
    ///   A valid file identifier of the sticker
    /// </summary>
    [djName('sticker_file_id')]
    FileId: string;
    constructor Create;
  end;

  /// <summary>
  ///   Represents a link to a video file stored on the Telegram servers. By
  ///   default, this video file will be sent by the user with an optional
  ///   caption. Alternatively, you can use input_message_content to send a
  ///   message with the specified content instead of the video.
  /// </summary>
  [djName('InlineQueryResultCachedVideo')]
  TtgInlineQueryResultCachedVideo = class(TtgInlineQueryResultCached)
  public
    /// <summary>
    ///   A valid file identifier for the video
    /// </summary>
    [djName('video_file_id')]
    FileId: string;
    /// <summary>
    ///   Optional. Short description of the result
    /// </summary>
    [djName('description')]
    Description: string;
    constructor Create;
  end;

  /// <summary>
  ///   Represents a link to a voice message stored on the Telegram servers. By
  ///   default, this voice message will be sent by the user. Alternatively,
  ///   you can use input_message_content to send a message with the specified
  ///   content instead of the voice message.
  /// </summary>
  [djName('InlineQueryResultCachedVoice')]
  TtgInlineQueryResultCachedVoice = class(TtgInlineQueryResultCached)
  public
    /// <summary>
    ///   A valid file identifier for the voice message
    /// </summary>
    [djName('voice_file_id')]
    FileId: string;
    constructor Create;
  end;

  /// <summary>
  ///   Represents a contact with a phone number. By default, this contact will
  ///   be sent by the user. Alternatively, you can use input_message_content
  ///   to send a message with the specified content instead of the contact.
  /// </summary>
  [djName('InlineQueryResultContact')]
  TtgInlineQueryResultContact = class(TtgInlineQueryResultNew)
  public
    /// <summary>
    ///   Contact's phone number
    /// </summary>
    [djName('phone_number')]
    PhoneNumber: string;
    /// <summary>
    ///   Contact's first name
    /// </summary>
    [djName('first_name')]
    FirstName: string;
    /// <summary>
    ///   Optional. Contact's last name
    /// </summary>
    [djName('last_name')]
    LastName: string;
    constructor Create;
  end;

  /// <summary>
  ///   Represents a link to a file. By default, this file will be sent by the
  ///   user with an optional caption. Alternatively, you can use
  ///   input_message_content to send a message with the specified content
  ///   instead of the file. Currently, only .PDF and .ZIP files can be sent
  ///   using this method.
  /// </summary>
  [djName('InlineQueryResultDocument')]
  TtgInlineQueryResultDocument = class(TtgInlineQueryResultNew)
  public
    /// <summary>
    ///   Optional. Caption of the document to be sent, 0-200 characters
    /// </summary>
    [djName('caption')]
    Caption: string;
    /// <summary>
    ///   A valid URL for the file
    /// </summary>
    [djName('document_url')]
    Url: string;
    /// <summary>
    ///   Mime type of the content of the file, either “application/pdf” or
    ///   “application/zip”
    /// </summary>
    [djName('mime_type')]
    MimeType: string;
    /// <summary>
    ///   Optional. Short description of the result
    /// </summary>
    [djName('description')]
    Description: string;
    constructor Create;
  end;

  { TODO -oM.E.Sysoev -cNew Type : add InlineQueryResultGame }
  TtgInlineQueryResultGame = class
  public
    [djName('type')]
    &type: String;
    [djName('id')]
    id: String;
    /// <summary>
    ///   Short name of the game.
    /// </summary>
    [djName('game_short_name')]
    GameShortName: String;
    [djName('reply_markup')]
    reply_markup: TtgInlineKeyboardMarkup;
    constructor Create;
  end;

  /// <summary>
  ///   Represents a link to an animated GIF file. By default, this animated
  ///   GIF file will be sent by the user with optional caption. Alternatively,
  ///   you can use input_message_content to send a message with the specified
  ///   content instead of the animation.
  /// </summary>
  [djName('InlineQueryResultGif')]
  TtgInlineQueryResultGif = class(TtgInlineQueryResultNew)
  public
    /// <summary>
    ///   A valid URL for the GIF file. File size must not exceed 1MB
    /// </summary>
    [djName('gif_url')]
    Url: string;
    /// <summary>
    ///   Optional. Width of the GIF
    /// </summary>
    [djName('gif_width')]
    Width: Integer;
    /// <summary>
    ///   Optional. Height of the GIF
    /// </summary>
    [djName('gif_height')]
    Height: Integer;
    /// <summary>
    ///   Optional. Duration of the GIF
    /// </summary>
    [djName('gif_duration')]
    Duration: Integer;
    /// <summary>
    ///   Optional. Caption of the GIF file to be sent, 0-200 characters
    /// </summary>
    [djName('caption')]
    Caption: string;
    /// <summary>
    ///   URL of the static thumbnail for the result (jpeg or gif)
    /// </summary>
    [djName('thumb_url')]
    ThumbUrl: string;
    constructor Create;
  end;

  /// <summary>
  ///   Represents a location on a map. By default, the location will be sent
  ///   by the user. Alternatively, you can use input_message_content to send a
  ///   message with the specified content instead of the location.
  /// </summary>
  [djName('InlineQueryResultLocation')]
  TtgInlineQueryResultLocation = class(TtgInlineQueryResultNew)
  public
    /// <summary>
    ///   Location latitude in degrees
    /// </summary>
    [djName('latitude')]
    Latitude: Single;
    /// <summary>
    ///   Location longitude in degrees
    /// </summary>
    [djName('longitude')]
    Longitude: Single;
    constructor Create;
  end;

  /// <summary>
  ///   Represents a link to a video animation (H.264/MPEG-4 AVC video without
  ///   sound). By default, this animated MPEG-4 file will be sent by the user
  ///   with optional caption. Alternatively, you can use input_message_content
  ///   to send a message with the specified content instead of the animation.
  /// </summary>
  [djName('InlineQueryResultMpeg4Gif')]
  TtgInlineQueryResultMpeg4Gif = class(TtgInlineQueryResult)
  public
    /// <summary>
    ///   A valid URL for the MP4 file. File size must not exceed 1MB
    /// </summary>
    [djName('mpeg4_url')]
    Url: string;
    /// <summary>
    ///   Optional. Video width
    /// </summary>
    [djName('mpeg4_width')]
    Width: Integer;
    /// <summary>
    ///   Optional. Video height
    /// </summary>
    [djName('mpeg4_height')]
    Height: Integer;
    /// <summary>
    ///   Optional. Video height
    /// </summary>
    [djName('mpeg4_duration')]
    Duration: Integer;
    /// <summary>
    ///   Optional. Caption of the MPEG-4 file to be sent, 0-200 characters
    /// </summary>
    [djName('caption')]
    Caption: string;
    constructor Create;
  end;

  /// <summary>
  ///   Represents a link to a photo. By default, this photo will be sent by
  ///   the user with optional caption. Alternatively, you can use
  ///   input_message_content to send a message with the specified content
  ///   instead of the photo.
  /// </summary>
  [djName('InlineQueryResultPhoto')]
  TtgInlineQueryResultPhoto = class(TtgInlineQueryResultNew)
  public
    /// <summary>
    ///   A valid URL of the photo. Photo must be in jpeg format. Photo size
    ///   must not exceed 5MB
    /// </summary>
    [djName('photo_url')]
    Url: string;
    /// <summary>
    ///   Optional. Width of the photo
    /// </summary>
    [djName('photo_width')]
    Width: Integer;
    /// <summary>
    ///   Optional. Height of the photo
    /// </summary>
    [djName('photo_height')]
    Height: Integer;
    /// <summary>
    ///   Optional. Short description of the result
    /// </summary>
    [djName('description')]
    Description: string;
    /// <summary>
    ///   Optional. Caption of the photo to be sent, 0-200 characters
    /// </summary>
    [djName('caption')]
    Caption: string;
    constructor Create;
  end;

  /// <summary>
  ///   Represents a venue. By default, the venue will be sent by the user.
  ///   Alternatively, you can use input_message_content to send a message with
  ///   the specified content instead of the venue.
  /// </summary>
  [djName('InlineQueryResultVenue')]
  TtgInlineQueryResultVenue = class(TtgInlineQueryResultNew)
  public
    /// <summary>
    ///   Latitude of the venue location in degrees
    /// </summary>
    [djName('latitude')]
    Latitude: Single;
    /// <summary>
    ///   Longitude of the venue location in degrees
    /// </summary>
    [djName('longitude')]
    Longitude: Single;
    /// <summary>
    ///   Address of the venue
    /// </summary>
    [djName('address')]
    Address: string;
    /// <summary>
    ///   Optional. Foursquare identifier of the venue if known
    /// </summary>
    [djName('foursquare_id')]
    FoursquareId: string;
    constructor Create;
  end;

  /// <summary>
  ///   Represents a link to a page containing an embedded video player or a
  ///   video file. By default, this video file will be sent by the user with
  ///   an optional caption. Alternatively, you can use input_message_content
  ///   to send a message with the specified content instead of the video.
  /// </summary>
  [djName('InlineQueryResultVideo')]
  TtgInlineQueryResultVideo = class(TtgInlineQueryResultNew)
  public
    /// <summary>
    ///   A valid URL for the embedded video player or video file
    /// </summary>
    [djName('video_url')]
    Url: string;
    /// <summary>
    ///   Mime type of the content of video url, “text/html” or “video/mp4”
    /// </summary>
    [djName('mime_type')]
    MimeType: string;
    /// <summary>
    ///   Optional. Video width
    /// </summary>
    [djName('video_width')]
    Width: Integer;
    /// <summary>
    ///   Optional. Video height
    /// </summary>
    [djName('video_height')]
    Height: Integer;
    /// <summary>
    ///   Optional. Video duration in seconds
    /// </summary>
    [djName('video_duration')]
    Duration: Integer;
    /// <summary>
    ///   Optional. Short description of the result
    /// </summary>
    [djName('description')]
    Description: string;
    /// <summary>
    ///   Optional. Caption of the video to be sent, 0-200 characters
    /// </summary>
    [djName('caption')]
    Caption: string;
    constructor Create;
  end;

  /// <summary>
  ///   Represents a link to a voice recording in an .ogg container encoded
  ///   with OPUS. By default, this voice recording will be sent by the user.
  ///   Alternatively, you can use input_message_content to send a message with
  ///   the specified content instead of the the voice message.
  /// </summary>
  [djName('InlineQueryResultVoice')]
  TtgInlineQueryResultVoice = class(TtgInlineQueryResultNew)
  public
    /// <summary>
    ///   A valid URL for the voice recording
    /// </summary>
    [djName('voice_url')]
    Url: string;
    /// <summary>
    ///   Optional. Recording duration in seconds
    /// </summary>
    [djName('voice_duration')]
    Duration: Integer;
    constructor Create;
  end;

implementation

uses
  System.SysUtils;

{ TtgInlineQueryResult }

constructor TtgInlineQueryResult.Create;
begin
  InputMessageContent := TtgInputMessageContent.Create;
  ReplyMarkup := TtgInlineKeyboardMarkup.Create;
end;

destructor TtgInlineQueryResult.Destroy;
begin
  FreeAndNil(ReplyMarkup);
  FreeAndNil(InputMessageContent);
  inherited;
end;

{ TtgInlineQueryResultArticle }

constructor TtgInlineQueryResultArticle.Create;
begin
  inherited;
  &Type := 'article';
end;

{ TtgInlineQueryResultAudio }

constructor TtgInlineQueryResultAudio.Create;
begin
  inherited;
  &Type := 'audio';
end;

{ TtgInlineQueryResultCachedAudio }

constructor TtgInlineQueryResultCachedAudio.Create;
begin
  inherited;
  &Type := 'audio';
end;

{ TTgInlineQueryResultCachedDocument }

constructor TTgInlineQueryResultCachedDocument.Create;
begin
  inherited;
  &Type := 'document';
end;

{ TtgInlineQueryResultCachedGif }

constructor TtgInlineQueryResultCachedGif.Create;
begin
  inherited;
  &Type := 'gif';
end;

{ TtgInlineQueryResultCachedMpeg4Gif }

constructor TtgInlineQueryResultCachedMpeg4Gif.Create;
begin
  inherited;
  &Type := 'mpeg4_gif';
end;

{ TtgInlineQueryResultCachedPhoto }

constructor TtgInlineQueryResultCachedPhoto.Create;
begin
  inherited;
  &Type := 'photo';
end;

{ TtgInlineQueryResultCachedSticker }

constructor TtgInlineQueryResultCachedSticker.Create;
begin
  inherited;
  &Type := 'sticker';
end;

{ TtgInlineQueryResultCachedVideo }

constructor TtgInlineQueryResultCachedVideo.Create;
begin
  inherited;
  &Type := 'video';
end;

{ TtgInlineQueryResultCachedVoice }

constructor TtgInlineQueryResultCachedVoice.Create;
begin
  inherited;
  &Type := 'voice';
end;

{ TtgInlineQueryResultContact }

constructor TtgInlineQueryResultContact.Create;
begin
  inherited;
  &Type := 'contact';
end;

{ TtgInlineQueryResultDocument }

constructor TtgInlineQueryResultDocument.Create;
begin
  inherited;
  &Type := 'document';
end;

{ TtgInlineQueryResultGame }

constructor TtgInlineQueryResultGame.Create;
begin
  inherited;
  &Type := 'document';
end;

{ TtgInlineQueryResultGif }

constructor TtgInlineQueryResultGif.Create;
begin
  inherited;
  &Type := 'gif';
end;

{ TtgInlineQueryResultLocation }

constructor TtgInlineQueryResultLocation.Create;
begin
  inherited;
  &Type := 'location';
end;

{ TtgInlineQueryResultMpeg4Gif }

constructor TtgInlineQueryResultMpeg4Gif.Create;
begin
  inherited;
  &Type := 'mpeg4_gif';
end;

{ TtgInlineQueryResultPhoto }

constructor TtgInlineQueryResultPhoto.Create;
begin
  inherited;
  &Type := 'photo';
end;

{ TtgInlineQueryResultVenue }

constructor TtgInlineQueryResultVenue.Create;
begin
  inherited;
  &Type := 'venue';
end;

{ TtgInlineQueryResultVideo }

constructor TtgInlineQueryResultVideo.Create;
begin
  inherited;
  &Type := 'video';
end;

{ TtgInlineQueryResultVoice }

constructor TtgInlineQueryResultVoice.Create;
begin
  inherited;
  &Type := 'voice';
end;

end.
