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

unit TelegaPi.Types.InlineQueryResults;

interface

uses
  REST.Json.Types,
  TelegaPi.Types.ReplyMarkups,
  TelegaPi.Types.InputMessageContents;

type

  /// <summary>
  ///   This object represents one result of an inline query. Telegram clients
  ///   currently support results of the following 19 types ///
  /// </summary>
  TtgInlineQueryResult = class abstract
  public
    /// <summary>
    ///   Unique identifier for this result, 1-64 bytes
    /// </summary>
    [JSONName('id')]
    ID: string;
    /// <summary>
    ///   Type of the result
    /// </summary>
    [JSONName('type')]
    &Type: string;
    /// <summary>
    ///   Title of the result
    /// </summary>
    [JSONName('title')]
    Title: String;
    /// <summary>
    ///   Optional. Inline keyboard attached to the message
    /// </summary>
    [JSONName('input_message_content')]
    InputMessageContent: TtgInputMessageContent;
    /// <summary>
    ///   Optional. Inline keyboard attached to the message
    /// </summary>
    [JSONName('reply_markup')]
    ReplyMarkup: TtgInlineKeyboardMarkup;
    constructor Create;
    destructor Destroy; override;
  end;

  TtgInlineQueryResultNew = class(TtgInlineQueryResult)
  public
    /// <summary>
    ///   Optional. Url of the thumbnail for the result
    /// </summary>
    [JSONName('thumb_url')]
    ThumbUrl: string;
    /// <summary>
    ///   Optional. Thumbnail width
    /// </summary>
    [JSONName('thumb_width')]
    ThumbWidth: Integer;
    /// <summary>
    ///   Optional. Thumbnail height
    /// </summary>
    [JSONName('thumb_height')]
    ThumbHeight: Integer;
  end;

  /// <summary>
  ///   Represents a link to an article or web page.
  /// </summary>
  [JSONName('InlineQueryResultArticle')]
  TtgInlineQueryResultArticle = class(TtgInlineQueryResultNew)
  public
    /// <summary>
    ///   Optional. URL of the result
    /// </summary>
    [JSONName('url')]
    Url: string;
    /// <summary>
    ///   Optional. Pass True, if you don't want the URL to be shown in the
    ///   message
    /// </summary>
    [JSONName('hide_url')]
    HideUrl: Boolean;
    /// <summary>
    ///   Optional. Short description of the result
    /// </summary>
    [JSONName('description')]
    Description: string;
    constructor Create;
  end;

  /// <summary>
  ///   Represents a link to an mp3 audio file. By default, this audio file
  ///   will be sent by the user. Alternatively, you can use
  ///   input_message_content to send a message with the specified content
  ///   instead of the audio.
  /// </summary>
  [JSONName('InlineQueryResultAudio')]
  TtgInlineQueryResultAudio = class(TtgInlineQueryResultNew)
  public
    /// <summary>
    ///   A valid file identifier for the audio file
    /// </summary>
    [JSONName('audio_file_id')]
    FileId: string;
    /// <summary>
    ///   A valid URL for the audio file
    /// </summary>
    [JSONName('audio_url')]
    Url: string;
    /// <summary>
    ///   Optional. Performer
    /// </summary>
    [JSONName('performer')]
    Performer: string;
    /// <summary>
    ///   Optional. Audio duration in seconds
    /// </summary>
    [JSONName('audio_duration')]
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
    [JSONName('caption')]
    Caption: string;
  end;

  /// <summary>
  ///   Represents a link to an mp3 audio file stored on the Telegram servers.
  ///   By default, this audio file will be sent by the user. Alternatively,
  ///   you can use input_message_content to send a message with the specified
  ///   content instead of the audio.
  /// </summary>
  [JSONName('InlineQueryResultCachedAudio')]
  TtgInlineQueryResultCachedAudio = class(TtgInlineQueryResultCached)
  public
    /// <summary>
    ///   A valid file identifier for the audio file
    /// </summary>
    [JSONName('audio_file_id')]
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
  [JSONName('InlineQueryResultCachedDocument')]
  TTgInlineQueryResultCachedDocument = class(TtgInlineQueryResultCached)
  public
    /// <summary>
    ///   A valid file identifier for the file
    /// </summary>
    [JSONName('document_file_id')]
    FileId: string;
    /// <summary>
    ///   Optional. Short description of the result
    /// </summary>
    [JSONName('description')]
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
  [JSONName('InlineQueryResultCachedGif')]
  TtgInlineQueryResultCachedGif = class(TtgInlineQueryResultCached)
  public
    /// <summary>
    ///   A valid file identifier for the GIF file
    /// </summary>
    [JSONName('gif_file_id')]
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
  [JSONName('InlineQueryResultCachedMpeg4Gif')]
  TtgInlineQueryResultCachedMpeg4Gif = class(TtgInlineQueryResultCached)
  public
    /// <summary>
    ///   A valid file identifier for the MP4 file
    /// </summary>
    [JSONName('mpeg4_file_id')]
    FileId: string;
    constructor Create;
  end;

  /// <summary>
  ///   Represents a link to a photo stored on the Telegram servers. By
  ///   default, this photo will be sent by the user with an optional caption.
  ///   Alternatively, you can use input_message_content to send a message with
  ///   the specified content instead of the photo.
  /// </summary>
  [JSONName('InlineQueryResultCachedPhoto')]
  TtgInlineQueryResultCachedPhoto = class(TtgInlineQueryResultCached)
  public
    /// <summary>
    ///   A valid file identifier of the photo
    /// </summary>
    [JSONName('photo_file_id')]
    FileId: string;
    /// <summary>
    ///   Optional. Short description of the result
    /// </summary>
    [JSONName('description')]
    Description: string;
    constructor Create;
  end;

  /// <summary>
  ///   Represents a link to a sticker stored on the Telegram servers. By
  ///   default, this sticker will be sent by the user. Alternatively, you can
  ///   use input_message_content to send a message with the specified content
  ///   instead of the sticker.
  /// </summary>
  [JSONName('InlineQueryResultCachedSticker')]
  TtgInlineQueryResultCachedSticker = class(TtgInlineQueryResultCached)
  public
    /// <summary>
    ///   A valid file identifier of the sticker
    /// </summary>
    [JSONName('sticker_file_id')]
    FileId: string;
    constructor Create;
  end;

  /// <summary>
  ///   Represents a link to a video file stored on the Telegram servers. By
  ///   default, this video file will be sent by the user with an optional
  ///   caption. Alternatively, you can use input_message_content to send a
  ///   message with the specified content instead of the video.
  /// </summary>
  [JSONName('InlineQueryResultCachedVideo')]
  TtgInlineQueryResultCachedVideo = class(TtgInlineQueryResultCached)
  public
    /// <summary>
    ///   A valid file identifier for the video
    /// </summary>
    [JSONName('video_file_id')]
    FileId: string;
    /// <summary>
    ///   Optional. Short description of the result
    /// </summary>
    [JSONName('description')]
    Description: string;
    constructor Create;
  end;

  /// <summary>
  ///   Represents a link to a voice message stored on the Telegram servers. By
  ///   default, this voice message will be sent by the user. Alternatively,
  ///   you can use input_message_content to send a message with the specified
  ///   content instead of the voice message.
  /// </summary>
  [JSONName('InlineQueryResultCachedVoice')]
  TtgInlineQueryResultCachedVoice = class(TtgInlineQueryResultCached)
  public
    /// <summary>
    ///   A valid file identifier for the voice message
    /// </summary>
    [JSONName('voice_file_id')]
    FileId: string;
    constructor Create;
  end;

  /// <summary>
  ///   Represents a contact with a phone number. By default, this contact will
  ///   be sent by the user. Alternatively, you can use input_message_content
  ///   to send a message with the specified content instead of the contact.
  /// </summary>
  [JSONName('InlineQueryResultContact')]
  TtgInlineQueryResultContact = class(TtgInlineQueryResultNew)
  public
    /// <summary>
    ///   Contact's phone number
    /// </summary>
    [JSONName('phone_number')]
    PhoneNumber: string;
    /// <summary>
    ///   Contact's first name
    /// </summary>
    [JSONName('first_name')]
    FirstName: string;
    /// <summary>
    ///   Optional. Contact's last name
    /// </summary>
    [JSONName('last_name')]
    LastName: string;
    [JSONName('vCard')]
    vCard: string;
    constructor Create;
  end;

  /// <summary>
  ///   Represents a link to a file. By default, this file will be sent by the
  ///   user with an optional caption. Alternatively, you can use
  ///   input_message_content to send a message with the specified content
  ///   instead of the file. Currently, only .PDF and .ZIP files can be sent
  ///   using this method.
  /// </summary>
  [JSONName('InlineQueryResultDocument')]
  TtgInlineQueryResultDocument = class(TtgInlineQueryResultNew)
  public
    /// <summary>
    ///   Optional. Caption of the document to be sent, 0-200 characters
    /// </summary>
    [JSONName('caption')]
    Caption: string;
    /// <summary>
    ///   A valid URL for the file
    /// </summary>
    [JSONName('document_url')]
    Url: string;
    /// <summary>
    ///   Mime type of the content of the file, either “application/pdf” or
    ///   “application/zip”
    /// </summary>
    [JSONName('mime_type')]
    MimeType: string;
    /// <summary>
    ///   Optional. Short description of the result
    /// </summary>
    [JSONName('description')]
    Description: string;
    constructor Create;
  end;

  { TODO -oM.E.Sysoev -cNew Type : add InlineQueryResultGame }
  TtgInlineQueryResultGame = class
  public
    [JSONName('type')]
    &Type: String;
    [JSONName('id')]
    ID: String;
    /// <summary>
    ///   Short name of the game.
    /// </summary>
    [JSONName('game_short_name')]
    GameShortName: String;
    [JSONName('reply_markup')]
    reply_markup: TtgInlineKeyboardMarkup;
    constructor Create;
  end;

  /// <summary>
  ///   Represents a link to an animated GIF file. By default, this animated
  ///   GIF file will be sent by the user with optional caption. Alternatively,
  ///   you can use input_message_content to send a message with the specified
  ///   content instead of the animation.
  /// </summary>
  [JSONName('InlineQueryResultGif')]
  TtgInlineQueryResultGif = class(TtgInlineQueryResultNew)
  public
    /// <summary>
    ///   A valid URL for the GIF file. File size must not exceed 1MB
    /// </summary>
    [JSONName('gif_url')]
    Url: string;
    /// <summary>
    ///   Optional. Width of the GIF
    /// </summary>
    [JSONName('gif_width')]
    Width: Integer;
    /// <summary>
    ///   Optional. Height of the GIF
    /// </summary>
    [JSONName('gif_height')]
    Height: Integer;
    /// <summary>
    ///   Optional. Duration of the GIF
    /// </summary>
    [JSONName('gif_duration')]
    Duration: Integer;
    /// <summary>
    ///   Optional. Caption of the GIF file to be sent, 0-200 characters
    /// </summary>
    [JSONName('caption')]
    Caption: string;
    /// <summary>
    ///   URL of the static thumbnail for the result (jpeg or gif)
    /// </summary>
    [JSONName('thumb_url')]
    ThumbUrl: string;
    constructor Create;
  end;

  /// <summary>
  ///   Represents a location on a map. By default, the location will be sent
  ///   by the user. Alternatively, you can use input_message_content to send a
  ///   message with the specified content instead of the location.
  /// </summary>
  [JSONName('InlineQueryResultLocation')]
  TtgInlineQueryResultLocation = class(TtgInlineQueryResultNew)
  public
    /// <summary>
    ///   Location latitude in degrees
    /// </summary>
    [JSONName('latitude')]
    Latitude: Single;
    /// <summary>
    ///   Location longitude in degrees
    /// </summary>
    [JSONName('longitude')]
    Longitude: Single;
    constructor Create;
  end;

  /// <summary>
  ///   Represents a link to a video animation (H.264/MPEG-4 AVC video without
  ///   sound). By default, this animated MPEG-4 file will be sent by the user
  ///   with optional caption. Alternatively, you can use input_message_content
  ///   to send a message with the specified content instead of the animation.
  /// </summary>
  [JSONName('InlineQueryResultMpeg4Gif')]
  TtgInlineQueryResultMpeg4Gif = class(TtgInlineQueryResult)
  public
    /// <summary>
    ///   A valid URL for the MP4 file. File size must not exceed 1MB
    /// </summary>
    [JSONName('mpeg4_url')]
    Url: string;
    /// <summary>
    ///   Optional. Video width
    /// </summary>
    [JSONName('mpeg4_width')]
    Width: Integer;
    /// <summary>
    ///   Optional. Video height
    /// </summary>
    [JSONName('mpeg4_height')]
    Height: Integer;
    /// <summary>
    ///   Optional. Video height
    /// </summary>
    [JSONName('mpeg4_duration')]
    Duration: Integer;
    /// <summary>
    ///   Optional. Caption of the MPEG-4 file to be sent, 0-200 characters
    /// </summary>
    [JSONName('caption')]
    Caption: string;
    constructor Create;
  end;

  /// <summary>
  ///   Represents a link to a photo. By default, this photo will be sent by
  ///   the user with optional caption. Alternatively, you can use
  ///   input_message_content to send a message with the specified content
  ///   instead of the photo.
  /// </summary>
  [JSONName('InlineQueryResultPhoto')]
  TtgInlineQueryResultPhoto = class(TtgInlineQueryResultNew)
  public
    /// <summary>
    ///   A valid URL of the photo. Photo must be in jpeg format. Photo size
    ///   must not exceed 5MB
    /// </summary>
    [JSONName('photo_url')]
    Url: string;
    /// <summary>
    ///   Optional. Width of the photo
    /// </summary>
    [JSONName('photo_width')]
    Width: Integer;
    /// <summary>
    ///   Optional. Height of the photo
    /// </summary>
    [JSONName('photo_height')]
    Height: Integer;
    /// <summary>
    ///   Optional. Short description of the result
    /// </summary>
    [JSONName('description')]
    Description: string;
    /// <summary>
    ///   Optional. Caption of the photo to be sent, 0-200 characters
    /// </summary>
    [JSONName('caption')]
    Caption: string;
    constructor Create;
  end;

  /// <summary>
  ///   Represents a venue. By default, the venue will be sent by the user.
  ///   Alternatively, you can use input_message_content to send a message with
  ///   the specified content instead of the venue.
  /// </summary>
  [JSONName('InlineQueryResultVenue')]
  TtgInlineQueryResultVenue = class(TtgInlineQueryResultNew)
  public
    /// <summary>
    ///   Latitude of the venue location in degrees
    /// </summary>
    [JSONName('latitude')]
    Latitude: Single;
    /// <summary>
    ///   Longitude of the venue location in degrees
    /// </summary>
    [JSONName('longitude')]
    Longitude: Single;
    /// <summary>
    ///   Address of the venue
    /// </summary>
    [JSONName('address')]
    Address: string;
    /// <summary>
    ///   Optional. Foursquare identifier of the venue if known
    /// </summary>
    [JSONName('foursquare_id')]
    FoursquareId: string;
    [JSONName('foursquare_type')]
    FoursquareType: string;
    constructor Create;
  end;

  /// <summary>
  ///   Represents a link to a page containing an embedded video player or a
  ///   video file. By default, this video file will be sent by the user with
  ///   an optional caption. Alternatively, you can use input_message_content
  ///   to send a message with the specified content instead of the video.
  /// </summary>
  [JSONName('InlineQueryResultVideo')]
  TtgInlineQueryResultVideo = class(TtgInlineQueryResultNew)
  public
    /// <summary>
    ///   A valid URL for the embedded video player or video file
    /// </summary>
    [JSONName('video_url')]
    Url: string;
    /// <summary>
    ///   Mime type of the content of video url, “text/html” or “video/mp4”
    /// </summary>
    [JSONName('mime_type')]
    MimeType: string;
    /// <summary>
    ///   Optional. Video width
    /// </summary>
    [JSONName('video_width')]
    Width: Integer;
    /// <summary>
    ///   Optional. Video height
    /// </summary>
    [JSONName('video_height')]
    Height: Integer;
    /// <summary>
    ///   Optional. Video duration in seconds
    /// </summary>
    [JSONName('video_duration')]
    Duration: Integer;
    /// <summary>
    ///   Optional. Short description of the result
    /// </summary>
    [JSONName('description')]
    Description: string;
    /// <summary>
    ///   Optional. Caption of the video to be sent, 0-200 characters
    /// </summary>
    [JSONName('caption')]
    Caption: string;
    constructor Create;
  end;

  /// <summary>
  ///   Represents a link to a voice recording in an .ogg container encoded
  ///   with OPUS. By default, this voice recording will be sent by the user.
  ///   Alternatively, you can use input_message_content to send a message with
  ///   the specified content instead of the the voice message.
  /// </summary>
  [JSONName('InlineQueryResultVoice')]
  TtgInlineQueryResultVoice = class(TtgInlineQueryResultNew)
  public
    /// <summary>
    ///   A valid URL for the voice recording
    /// </summary>
    [JSONName('voice_url')]
    Url: string;
    /// <summary>
    ///   Optional. Recording duration in seconds
    /// </summary>
    [JSONName('voice_duration')]
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
