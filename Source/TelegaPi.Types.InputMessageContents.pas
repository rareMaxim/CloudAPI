unit TelegaPi.Types.InputMessageContents;

interface

uses
  DJSON.Attributes;

type

  /// <summary>
  ///   This object represents the content of a message to be sent as a result
  ///   of an inline query.
  /// </summary>
  [djName('InputMessageContent')]
  TtgInputMessageContent = class
  end;

  /// <summary>
  ///   Represents the content of a contact message to be sent as the result of
  ///   an inline query.
  /// </summary>
  [djName('InputContactMessageContent')]
  TtgInputContactMessageContent = class(TtgInputMessageContent)
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
  end;

  /// <summary>
  ///   Represents the content of a location message to be sent as the result
  ///   of an inline query.
  /// </summary>
  [djName('InputLocationMessageContent')]
  TtgInputLocationMessageContent = class(TtgInputMessageContent)
  public
    /// <summary>
    ///   Latitude of the location in degrees
    /// </summary>
    [djName('latitude')]
    Latitude: Single;
    /// <summary>
    ///   Longitude of the location in degrees
    /// </summary>
    [djName('longitude')]
    Longitude: Single;
  end;

  /// <summary>
  ///   Represents the content of a text message to be sent as the result of an
  ///   inline query.
  /// </summary>
  [djName('InputTextMessageContent')]
  TtgInputTextMessageContent = class(TtgInputMessageContent)
  public
    /// <summary>
    ///   Text of the message to be sent, 1-4096 characters
    /// </summary>
    [djName('message_text')]
    MessageText: string;
    /// <summary>
    ///   Optional. Send Markdown or HTML, if you want Telegram apps to show
    ///   bold, italic, fixed-width text or inline URLs in your bot's message.
    /// </summary>
    [djName('parse_mode')]
    ParseMode: string;
    /// <summary>
    ///   Optional. Disables link previews for links in the sent message
    /// </summary>
    [djName('disable_web_page_preview')]
    DisableWebPagePreview: Boolean;
  end;

  /// <summary>
  ///   Represents the content of a venue message to be sent as the result of
  ///   an inline query.
  /// </summary>
  [djName('InputVenueMessageContent')]
  TtgInputVenueMessageContent = class(TtgInputMessageContent)
  public
    /// <summary>
    ///   Latitude of the venue in degrees
    /// </summary>
    [djName('latitude')]
    Latitude: Single;
    /// <summary>
    ///   Longitude of the venue in degrees
    /// </summary>
    [djName('longitude')]
    Longitude: Single;
    /// <summary>
    ///   Name of the venue
    /// </summary>
    [djName('title')]
    Title: string;
    /// <summary>
    ///   Address of the venue
    /// </summary>
    [djName('address')]
    Address: string;
    /// <summary>
    ///   Optional. Foursquare identifier of the venue, if known
    /// </summary>
    [djName('foursquare_id')]
    FoursquareId: string;
  end;

implementation

end.

