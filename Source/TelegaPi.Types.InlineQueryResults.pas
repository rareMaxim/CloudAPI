unit TelegaPi.Types.InlineQueryResults;

interface

uses
  XSuperObject,
  TelegaPi.Types.ReplyMarkups,
  TelegaPi.Types.InputMessageContents;

type

  /// <summary>This object represents one result of an inline query. Telegram clients currently support results of the following 19 types   /// </summary>
  [Alias('InlineQueryResult')]
  TtgInlineQueryResult = class
  public
    destructor Destroy; override;
  public
    /// <summary>
    /// Unique identifier for this result, 1-64 bytes
    /// </summary>
    [Alias('id')]
    ID: string;
    /// <summary>
    /// Type of the result
    /// </summary>
    [Alias('type')]
    &Type: string;
    /// <summary>
    /// Title of the result
    /// </summary>
    [Alias('title')]
    Title: String;
    /// <summary>
    /// Optional. Inline keyboard attached to the message
    /// </summary>
    [Alias('input_message_content')]
    InputMessageContent: TtgInputMessageContent;
    /// <summary>
    /// Optional. Inline keyboard attached to the message
    /// </summary>
    [Alias('reply_markup')]
    ReplyMarkup: TtgInlineKeyboardMarkup;
  end;

  TtgInlineQueryResultNew = class(TtgInlineQueryResult)
  public
    /// <summary>
    /// Optional. Url of the thumbnail for the result
    /// </summary>
    [Alias('thumb_url')]
    ThumbUrl: string;
    /// <summary>
    /// Optional. Thumbnail width
    /// </summary>
    [Alias('thumb_width')]
    ThumbWidth: Integer;
    /// <summary>
    /// Optional. Thumbnail height
    /// </summary>
    [Alias('thumb_height')]
    ThumbHeight: Integer;
  end;

  /// <summary>
  /// Represents a link to an article or web page.
  /// </summary>
  [Alias('InlineQueryResultArticle')]
  TtgInlineQueryResultArticle = class(TtgInlineQueryResultNew)
  public
    constructor Create;
  public
    /// <summary>
    /// Optional. URL of the result
    /// </summary>
    [Alias('url')]
    Url: string;
    /// <summary>
    /// Optional. Pass True, if you don't want the URL to be shown in the
    /// message
    /// </summary>
    [Alias('hide_url')]
    HideUrl: Boolean;
    /// <summary>
    /// Optional. Short description of the result
    /// </summary>
    [Alias('description')]
    Description: string;
  end;

  /// <summary>
  /// Represents a link to an mp3 audio file. By default, this audio file
  /// will be sent by the user. Alternatively, you can use
  /// input_message_content to send a message with the specified content
  /// instead of the audio.
  /// </summary>
  [Alias('InlineQueryResultAudio')]
  TtgInlineQueryResultAudio = class(TtgInlineQueryResultNew)
  public
    /// <summary>
    /// A valid file identifier for the audio file
    /// </summary>
    [Alias('audio_file_id')]
    FileId: string;
    /// <summary>
    /// A valid URL for the audio file
    /// </summary>
    [Alias('audio_url')]
    Url: string;
    /// <summary>
    /// Optional. Performer
    /// </summary>
    [Alias('performer')]
    Performer: string;
    /// <summary>
    /// Optional. Audio duration in seconds
    /// </summary>
    [Alias('audio_duration')]
    Duration: Integer;
  end;

  /// <summary>
  /// Represents a link to a result stored on the Telegram servers. By default, this result will be sent by the user with an optional caption. Alternatively, you can use input_message_content to send a message with the specified content instead of the photo.
  /// </summary>
  TtgInlineQueryResultCached = class(TtgInlineQueryResult)
    /// <summary>
    /// Optional. Caption of the result to be sent, 0-200 characters
    /// </summary>
    [Alias('caption')]
    Caption: string;
  end;

  /// <summary>
  /// Represents a link to an mp3 audio file stored on the Telegram servers.
  /// By default, this audio file will be sent by the user. Alternatively,
  /// you can use input_message_content to send a message with the specified
  /// content instead of the audio.
  /// </summary>
  [Alias('InlineQueryResultCachedAudio')]
  TtgInlineQueryResultCachedAudio = class(TtgInlineQueryResultCached)
  public
    /// <summary>
    /// A valid file identifier for the audio file
    /// </summary>
    [Alias('audio_file_id')]
    FileId: string;
  end;

  /// <summary>
  /// Represents a link to a file stored on the Telegram servers. By default,
  /// this file will be sent by the user with an optional caption.
  /// Alternatively, you can use input_message_content to send a message with
  /// the specified content instead of the file. Currently, only pdf-files
  /// and zip archives can be sent using this method.
  /// </summary>
  [Alias('InlineQueryResultCachedDocument')]
  TTgInlineQueryResultCachedDocument = class(TtgInlineQueryResultCached)
  public
    /// <summary>
    /// A valid file identifier for the file
    /// </summary>
    [Alias('document_file_id')]
    FileId: string;
    /// <summary>
    /// Optional. Short description of the result
    /// </summary>
    [Alias('description')]
    Description: string;
  end;

  /// <summary>
  /// Represents a link to an animated GIF file stored on the Telegram
  /// servers. By default, this animated GIF file will be sent by the user
  /// with an optional caption. Alternatively, you can use
  /// input_message_content to send a message with specified content instead
  /// of the animation.
  /// </summary>
  [Alias('InlineQueryResultCachedGif')]
  TtgInlineQueryResultCachedGif = class(TtgInlineQueryResultCached)
  public
    /// <summary>
    /// A valid file identifier for the GIF file
    /// </summary>
    [Alias('gif_file_id')]
    FileId: string;
  end;

  /// <summary>
  /// Represents a link to a video animation (H.264/MPEG-4 AVC video without
  /// sound) stored on the Telegram servers. By default, this animated MPEG-4
  /// file will be sent by the user with an optional caption. Alternatively,
  /// you can use input_message_content to send a message with the specified
  /// content instead of the animation.
  /// </summary>
  [Alias('InlineQueryResultCachedMpeg4Gif')]
  TtgInlineQueryResultCachedMpeg4Gif = class(TtgInlineQueryResultCached)
  public
    /// <summary>
    /// A valid file identifier for the MP4 file
    /// </summary>
    [Alias('mpeg4_file_id')]
    FileId: string;
  end;

  /// <summary>
  /// Represents a link to a photo stored on the Telegram servers. By
  /// default, this photo will be sent by the user with an optional caption.
  /// Alternatively, you can use input_message_content to send a message with
  /// the specified content instead of the photo.
  /// </summary>
  [Alias('InlineQueryResultCachedPhoto')]
  TtgInlineQueryResultCachedPhoto = class(TtgInlineQueryResultCached)
  public
    /// <summary>
    /// A valid file identifier of the photo
    /// </summary>
    [Alias('photo_file_id')]
    FileId: string;
    /// <summary>
    /// Optional. Short description of the result
    /// </summary>
    [Alias('description')]
    Description: string;
  end;

  /// <summary>
  /// Represents a link to a sticker stored on the Telegram servers. By
  /// default, this sticker will be sent by the user. Alternatively, you can
  /// use input_message_content to send a message with the specified content
  /// instead of the sticker.
  /// </summary>
  [Alias('InlineQueryResultCachedSticker')]
  TtgInlineQueryResultCachedSticker = class(TtgInlineQueryResultCached)
  public
    /// <summary>
    /// A valid file identifier of the sticker
    /// </summary>
    [Alias('sticker_file_id')]
    FileId: string;
  end;

  /// <summary>
  /// Represents a link to a video file stored on the Telegram servers. By
  /// default, this video file will be sent by the user with an optional
  /// caption. Alternatively, you can use input_message_content to send a
  /// message with the specified content instead of the video.
  /// </summary>
  [Alias('InlineQueryResultCachedVideo')]
  TtgInlineQueryResultCachedVideo = class(TtgInlineQueryResultCached)
  public
    /// <summary>
    /// A valid file identifier for the video
    /// </summary>
    [Alias('video_file_id')]
    FileId: string;
    /// <summary>
    /// Optional. Short description of the result
    /// </summary>
    [Alias('description')]
    Description: string;
  end;

  /// <summary>
  /// Represents a link to a voice message stored on the Telegram servers. By
  /// default, this voice message will be sent by the user. Alternatively,
  /// you can use input_message_content to send a message with the specified
  /// content instead of the voice message.
  /// </summary>
  [Alias('InlineQueryResultCachedVoice')]
  TtgInlineQueryResultCachedVoice = class(TtgInlineQueryResultCached)
  public
    /// <summary>
    /// A valid file identifier for the voice message
    /// </summary>
    [Alias('voice_file_id')]
    FileId: string;
  end;

  /// <summary>
  /// Represents a contact with a phone number. By default, this contact will
  /// be sent by the user. Alternatively, you can use input_message_content
  /// to send a message with the specified content instead of the contact.
  /// </summary>
  [Alias('InlineQueryResultContact')]
  TtgInlineQueryResultContact = class(TtgInlineQueryResultNew)
  public
    /// <summary>
    /// Contact's phone number
    /// </summary>
    [Alias('phone_number')]
    PhoneNumber: string;
    /// <summary>
    /// Contact's first name
    /// </summary>
    [Alias('first_name')]
    FirstName: string;
    /// <summary>
    /// Optional. Contact's last name
    /// </summary>
    [Alias('last_name')]
    LastName: string;
  end;

  /// <summary>
  /// Represents a link to a file. By default, this file will be sent by the
  /// user with an optional caption. Alternatively, you can use
  /// input_message_content to send a message with the specified content
  /// instead of the file. Currently, only .PDF and .ZIP files can be sent
  /// using this method.
  /// </summary>
  [Alias('InlineQueryResultDocument')]
  TtgInlineQueryResultDocument = class(TtgInlineQueryResultNew)
  public
    /// <summary>
    /// Optional. Caption of the document to be sent, 0-200 characters
    /// </summary>
    [Alias('caption')]
    Caption: string;
    /// <summary>
    /// A valid URL for the file
    /// </summary>
    [Alias('document_url')]
    Url: string;
    /// <summary>
    /// Mime type of the content of the file, either “application/pdf” or
    /// “application/zip”
    /// </summary>
    [Alias('mime_type')]
    MimeType: string;
    /// <summary>
    /// Optional. Short description of the result
    /// </summary>
    [Alias('description')]
    Description: string;
  end;

  { TODO -oM.E.Sysoev -cNew Type : add InlineQueryResultGame }
  TtgInlineQueryResultGame = class
  public
    /// <summary>
    /// Short name of the game.
    /// </summary>
    [Alias('game_short_name')]
    GameShortName: String;
  end;

  /// <summary>
  /// Represents a link to an animated GIF file. By default, this animated
  /// GIF file will be sent by the user with optional caption. Alternatively,
  /// you can use input_message_content to send a message with the specified
  /// content instead of the animation.
  /// </summary>
  [Alias('InlineQueryResultGif')]
  TtgInlineQueryResultGif = class(TtgInlineQueryResultNew)
  public
    /// <summary>
    /// A valid URL for the GIF file. File size must not exceed 1MB
    /// </summary>
    [Alias('gif_url')]
    Url: string;
    /// <summary>
    /// Optional. Width of the GIF
    /// </summary>
    [Alias('gif_width')]
    Width: Integer;
    /// <summary>
    /// Optional. Height of the GIF
    /// </summary>
    [Alias('gif_height')]
    Height: Integer;
    /// <summary>
    /// Optional. Duration of the GIF
    /// </summary>
    [Alias('gif_duration')]
    Duration: Integer;
    /// <summary>
    /// Optional. Caption of the GIF file to be sent, 0-200 characters
    /// </summary>
    [Alias('caption')]
    Caption: string;
    /// <summary>
    /// URL of the static thumbnail for the result (jpeg or gif)
    /// </summary>
    [Alias('thumb_url')]
    Thumb_url: string;
  end;

  /// <summary>
  /// Represents a location on a map. By default, the location will be sent
  /// by the user. Alternatively, you can use input_message_content to send a
  /// message with the specified content instead of the location.
  /// </summary>
  [Alias('InlineQueryResultLocation')]
  TtgInlineQueryResultLocation = class(TtgInlineQueryResultNew)
  public
    /// <summary>
    /// Location latitude in degrees
    /// </summary>
    [Alias('latitude')]
    Latitude: Single;
    /// <summary>
    /// Location longitude in degrees
    /// </summary>
    [Alias('longitude')]
    Longitude: Single;
  end;

  /// <summary>Represents a link to a video animation (H.264/MPEG-4 AVC video without sound). By default, this animated MPEG-4 file will be sent by the user with optional caption. Alternatively, you can use input_message_content to send a message with the specified content instead of the animation.</summary>
  [Alias('InlineQueryResultMpeg4Gif')]
  TtgInlineQueryResultMpeg4Gif = class(TtgInlineQueryResult)
  public
    /// <summary>
    /// A valid URL for the MP4 file. File size must not exceed 1MB
    /// </summary>
    [Alias('mpeg4_url')]
    Url: string;
    /// <summary>
    /// Optional. Video width
    /// </summary>
    [Alias('mpeg4_width')]
    Width: Integer;
    /// <summary>
    /// Optional. Video height
    /// </summary>
    [Alias('mpeg4_height')]
    Height: Integer;
    /// <summary>
    /// Optional. Video height
    /// </summary>
    [Alias('mpeg4_duration')]
    Duration: Integer;
    /// <summary>
    /// Optional. Caption of the MPEG-4 file to be sent, 0-200 characters
    /// </summary>
    [Alias('caption')]
    Caption: string;
  end;

  /// <summary>
  /// Represents a link to a photo. By default, this photo will be sent by
  /// the user with optional caption. Alternatively, you can use
  /// input_message_content to send a message with the specified content
  /// instead of the photo.
  /// </summary>
  [Alias('InlineQueryResultPhoto')]
  TtgInlineQueryResultPhoto = class(TtgInlineQueryResultNew)
  public
    /// <summary>
    /// A valid URL of the photo. Photo must be in jpeg format. Photo size
    /// must not exceed 5MB
    /// </summary>
    [Alias('photo_url')]
    Url: string;
    /// <summary>
    /// Optional. Width of the photo
    /// </summary>
    [Alias('photo_width')]
    Width: Integer;
    /// <summary>
    /// Optional. Height of the photo
    /// </summary>
    [Alias('photo_height')]
    Height: Integer;
    /// <summary>
    /// Optional. Short description of the result
    /// </summary>
    [Alias('description')]
    Description: string;
    /// <summary>
    /// Optional. Caption of the photo to be sent, 0-200 characters
    /// </summary>
    [Alias('caption')]
    Caption: string;
  end;

  /// <summary>
  /// Represents a venue. By default, the venue will be sent by the user.
  /// Alternatively, you can use input_message_content to send a message with
  /// the specified content instead of the venue.
  /// </summary>
  [Alias('InlineQueryResultVenue')]
  TtgInlineQueryResultVenue = class(TtgInlineQueryResultNew)
  public
    /// <summary>
    /// Latitude of the venue location in degrees
    /// </summary>
    [Alias('latitude')]
    Latitude: Single;
    /// <summary>
    /// Longitude of the venue location in degrees
    /// </summary>
    [Alias('longitude')]
    Longitude: Single;
    /// <summary>
    /// Address of the venue
    /// </summary>
    [Alias('address')]
    Address: string;
    /// <summary>
    /// Optional. Foursquare identifier of the venue if known
    /// </summary>
    [Alias('foursquare_id')]
    FoursquareId: string;
  end;

  /// <summary>
  /// Represents a link to a page containing an embedded video player or a
  /// video file. By default, this video file will be sent by the user with
  /// an optional caption. Alternatively, you can use input_message_content
  /// to send a message with the specified content instead of the video.
  /// </summary>
  [Alias('InlineQueryResultVideo')]
  TtgInlineQueryResultVideo = class(TtgInlineQueryResultNew)
  public
    /// <summary>
    /// A valid URL for the embedded video player or video file
    /// </summary>
    [Alias('video_url')]
    Url: string;
    /// <summary>
    /// Mime type of the content of video url, “text/html” or “video/mp4”
    /// </summary>
    [Alias('mime_type')]
    MimeType: string;
    /// <summary>
    /// Optional. Video width
    /// </summary>
    [Alias('video_width')]
    Width: Integer;
    /// <summary>
    /// Optional. Video height
    /// </summary>
    [Alias('video_height')]
    Height: Integer;
    /// <summary>
    /// Optional. Video duration in seconds
    /// </summary>
    [Alias('video_duration')]
    Duration: Integer;
    /// <summary>
    /// Optional. Short description of the result
    /// </summary>
    [Alias('description')]
    Description: string;
    /// <summary>
    /// Optional. Caption of the video to be sent, 0-200 characters
    /// </summary>
    [Alias('caption')]
    Caption: string;
  end;

  /// <summary>
  /// Represents a link to a voice recording in an .ogg container encoded
  /// with OPUS. By default, this voice recording will be sent by the user.
  /// Alternatively, you can use input_message_content to send a message with
  /// the specified content instead of the the voice message.
  /// </summary>
  [Alias('InlineQueryResultVoice')]
  TtgInlineQueryResultVoice = class(TtgInlineQueryResultNew)
  public
    /// <summary>
    /// A valid URL for the voice recording
    /// </summary>
    [Alias('voice_url')]
    Url: string;
    /// <summary>
    /// Optional. Recording duration in seconds
    /// </summary>
    [Alias('voice_duration')]
    Duration: Integer;
  end;

implementation

uses
  System.SysUtils;

{ TtgInlineQueryResult }

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

end.
