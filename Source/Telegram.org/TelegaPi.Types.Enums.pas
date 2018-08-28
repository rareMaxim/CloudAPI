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

unit TelegaPi.Types.Enums;

interface

type

{$SCOPEDENUMS ON}
  /// <summary>
  /// Type of action to broadcast.
  /// </summary>
  /// <remarks>
  /// We only recommend using this method when a response from the bot will
  /// take a noticeable amount of time to arrive.
  /// </remarks>
  /// <example>
  /// Example: The ImageBot needs some time to process a request and upload
  /// the image. Instead of sending a text message along the lines of
  /// “Retrieving image, please wait…”, the bot may use sendChatAction with
  /// action = upload_photo. The user will see a “sending photo” status for
  /// the bot.
  /// </example>
  TtgSendChatAction = (
    /// <summary>
    /// for text messages
    /// </summary>
    Typing,
    /// <summary>
    /// for photos
    /// </summary>
    UploadPhoto,
    /// <summary>
    /// for videos
    /// </summary>
    Record_video,
    /// <summary>
    /// for videos
    /// </summary>
    UploadVideo,
    /// <summary>
    /// for audio files
    /// </summary>
    Record_audio,
    /// <summary>
    /// for audio files
    /// </summary>
    Upload_audio,
    /// <summary>
    /// for general files
    /// </summary>
    Upload_document,
    /// <summary>
    /// for location data
    /// </summary>
    Find_location,
    /// <summary>
    /// for video notes
    /// </summary>
    Record_video_note,
    /// <summary>
    /// for video notes
    /// </summary>
    Upload_video_note);

  /// <summary>
  /// ChatMember status
  /// </summary>
  TtgChatMemberStatus = (
    /// <summary>
    /// Creator of the <see cref="Chat" />
    /// </summary>
    Creator,

    /// <summary>
    /// Administrator of the <see cref="Chat" />
    /// </summary>
    Administrator,

    /// <summary>
    /// Normal member of the <see cref="Chat" />
    /// </summary>
    Member, Restricted,

    /// <summary>
    /// A <see cref="User" /> who left the <see cref="Chat" />
    /// </summary>
    Left,
    /// <summary>
    /// A <see cref="User" /> who was kicked from the <see cref="Chat" />
    /// </summary>
    Kicked);
  /// <summary>
  /// Type of a <see cref="Chat" />
  /// </summary>

  TtgChatType = (
    /// <summary>
    /// Normal one to one <see cref="Chat" />
    /// </summary>
    private,

    /// <summary>
    /// Normal groupchat
    /// </summary>
    Group,

    /// <summary>
    /// A channel
    /// </summary>
    Channel,

    /// <summary>
    /// A supergroup
    /// </summary>
    Supergroup);
  /// <summary>
  /// Type of a <see cref="FileToSend" />
  /// </summary>

  TtgFileType = (
    /// <summary>
    /// Unknown FileType
    /// </summary>
    Unknown,
    /// <summary>
    /// FileStream
    /// </summary>
    Stream,
    /// <summary>
    /// FileId
    /// </summary>
    Id,
    /// <summary>
    /// File Url
    /// </summary>
    Url);
  /// <summary>
  /// The type of a Message
  /// </summary>

  TtgMessageType = (Unknown = 0, Text, Photo, Audio, Video, VideoNote, Voice, Document, Sticker, Game, Location,
    Contact, Service, Venue);
  /// <summary>
  /// Text parsing mode
  /// </summary>
  /// <example>
  /// <para>
  /// Markdown style
  /// </para>
  /// <para>
  /// *bold text* <br />_italic text_ <br />
  /// [text](http://www.example.com/) <br />`inline fixed-width code` <br />
  /// ```text <br />pre-formatted fixed-width code block <br />```
  /// </para>
  /// <para>
  /// Html:
  /// </para>
  /// <para>
  /// &lt;b&gt;bold&lt;/b&gt;, &lt;strong&gt;bold&lt;/strong&gt; <br />
  /// &lt;i&gt;italic&lt;/i&gt;, &lt;em&gt;italic&lt;/em&gt; <br />&lt;a
  /// href="http://www.example.com/"&gt;inline URL&lt;/a&gt; <br />
  /// &lt;code&gt;inline fixed-width code&lt;/code&gt; <br />
  /// &lt;pre&gt;pre-formatted fixed-width code block&lt;/pre&gt; <br /><br />
  /// </para>
  /// </example>

  TtgParseMode = (default = 0,
    /// <summary>
    /// To use this mode, pass Markdown in the parse_mode field when using
    /// sendMessage
    /// </summary>
    Markdown,
    /// <summary>
    /// To use this mode, pass HTML in the parse_mode field when using
    /// sendMessage
    /// </summary>
    Html);

  /// <summary>
  /// The type of an Update
  /// </summary>
  TtgUpdateType = (
    /// <summary>
    /// Update Type is unknown
    /// </summary>
    UnknownUpdate = 0,

    /// <summary>
    /// The <see cref="Update" /> contains a <see cref="Message" />.
    /// </summary>
    MessageUpdate,

    /// <summary>
    /// The <see cref="Update" /> contains an <see cref="InlineQuery" />.
    /// </summary>
    InlineQueryUpdate,

    /// <summary>
    /// The <see cref="Update" /> contains a <see cref="ChosenInlineResult" />
    /// .
    /// </summary>
    ChosenInlineResultUpdate,

    /// <summary>
    /// The <see cref="Update" /> contins a <see cref="CallbackQuery" />
    /// </summary>
    CallbackQueryUpdate,

    /// <summary>
    /// The <see cref="Update" /> contains an edited <see cref="Message" />
    /// </summary>
    EditedMessage,

    /// <summary>
    /// The <see cref="Update" /> contains a channel post <see cref="Message" />
    /// </summary>
    ChannelPost,

    /// <summary>
    /// The <see cref="Update" /> contains an edited channel post <see cref="Message" />
    /// </summary>
    EditedChannelPost,

    /// <summary>
    /// The <see cref="Update" /> contains an <see cref="ShippingQueryUpdate" />
    /// </summary>
    ShippingQueryUpdate,

    /// <summary>
    /// The <see cref="Update" /> contains an <see cref="PreCheckoutQueryUpdate" />
    /// </summary>
    PreCheckoutQueryUpdate,

    /// <summary>
    /// Receive all <see cref="Update" /> Types
    /// </summary>
    All = 255);

  /// <summary>
  /// Type of a <see cref="MessageEntity" />
  /// </summary>
  TtgMessageEntityType = (

    /// <summary>
    /// A mentioned <see cref="User" />
    /// </summary>
    Mention,

    /// <summary>
    /// A searchable Hashtag
    /// </summary>
    Hashtag, CashTag,
    /// <summary>
    /// A Bot command
    /// </summary>
    BotCommand,

    /// <summary>
    /// An url
    /// </summary>
    Url,

    /// <summary>
    /// An email
    /// </summary>
    Email, PhoneNumber,

    /// <summary>
    /// Bold text
    /// </summary>
    Bold,

    /// <summary>
    /// Italic text
    /// </summary>
    Italic,

    /// <summary>
    /// Monowidth string
    /// </summary>
    Code,

    /// <summary>
    /// Monowidth block
    /// </summary>
    Pre,

    /// <summary>
    /// Clickable text urls
    /// </summary>
    TextLink,

    /// <summary>
    /// Mentions for a <see cref="User" /> without <see cref="User.Username" />
    /// </summary>
    TextMention);
  /// <summary>
  /// The part of the face relative to which the mask should be placed. One
  /// of “forehead”, “eyes”, “mouth”, or “chin”.
  /// </summary>

  TtgMaskPositionPoint = (
    /// <summary>
    /// The forehead
    /// </summary>
    forehead,
    /// <summary>
    /// The eyes
    /// </summary>
    eyes,
    /// <summary>
    /// The mouth
    /// </summary>
    mouth,
    /// <summary>
    /// The chin
    chin);

  TtgGender = (Male, Female);

  TtgPassportAvaibleData = (PersonalDetails, Passport, InternalPassport, DriverLicense, IdentityCard, IdDocument,
    IdSelfie, Address, UtilityBill, BankStatement, RentalAgreement, PassportRegistration, TemporaryRegistration,
    AdressDocument, PhoneNumber, Email);

  TAllowedUpdate = (message, Edited_message, Channel_post, Edited_channel_post, Inline_query, Chosen_inline_result,
    Callback_query);

  TAllowedUpdates = set of TAllowedUpdate;

const
  UPDATES_ALLOWED_ALL = [Low(TAllowedUpdate) .. High(TAllowedUpdate)];

implementation

end.
