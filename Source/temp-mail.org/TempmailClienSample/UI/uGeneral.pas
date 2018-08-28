unit uGeneral;

interface

uses
  TempmailAPI,

  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Rtti,
  FMX.Edit, FMX.StdCtrls, FMX.Layouts, FMX.Grid, FMX.Controls.Presentation,
  FMX.ListBox;

type
  TForm1 = class(TForm)
    ToolBar1: TToolBar;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Grid1: TGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    StringColumn3: TStringColumn;
    Column1: TColumn;
    Layout1: TLayout;
    Label1: TLabel;
    Edit1: TEdit;
    StyleBook1: TStyleBook;
    ComboBox1: TComboBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Grid1GetValue(Sender: TObject; const Col, Row: Integer;
      var Value: TValue);
  private
    { Private declarations }
    FTempMail: TTempMailClientAPI;
    procedure OnDomainsGet(Sender: TObject);
    procedure OnLettersGet(Sender: TObject);
    procedure OnDeleteGet(Sender: TObject);

  public
    { Public declarations }
    Function CurrentEmail: String;
  end;

var
  Form1: TForm1;

implementation

Uses
  FMX.Platform;
{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
var
  ClipboardService: IFMXClipboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService,
    ClipboardService) then
  begin
    ClipboardService.SetClipboard(CurrentEmail);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FTempMail.GetMailAsync(CurrentEmail);
end;

function TForm1.CurrentEmail: String;
begin
  Result := Edit1.Text + ComboBox1.Selected.Text;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FTempMail := TTempMailClientAPI.Create(Nil);
  FTempMail.OnGetDomains := OnDomainsGet;
  FTempMail.OnGetLetters := OnLettersGet;
  FTempMail.OnDelete := OnDeleteGet;

  FTempMail.getDomainsAsync;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FTempMail.Free;
end;

procedure TForm1.Grid1GetValue(Sender: TObject; const Col, Row: Integer;
  var Value: TValue);
begin
  if FTempMail.Letters.Count = 0 then
    Exit;
  if Row >= FTempMail.Letters.Count then
    Exit;
  Grid1.RowCount := FTempMail.Letters.Count;
  case Col of
    0:
      Value := FTempMail.Letters[Row].mail_from;
  end;
end;

procedure TForm1.OnDeleteGet(Sender: TObject);
begin
  //
end;

procedure TForm1.OnDomainsGet(Sender: TObject);
var
  Domain: String;
begin
  ComboBox1.BeginUpdate;
  try
    ComboBox1.Clear;
    for Domain in FTempMail.Domains do
    begin
      ComboBox1.Items.Add(Domain);
    end;
    ComboBox1.ItemIndex := 0;
  finally
    ComboBox1.EndUpdate;
  end;
end;

procedure TForm1.OnLettersGet(Sender: TObject);
begin
  Grid1.UpdateColumns;
end;

end.
