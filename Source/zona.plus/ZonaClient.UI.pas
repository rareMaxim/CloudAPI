unit ZonaClient.UI;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.ListBox,
  FMX.Layouts,
  FMX.Controls.Presentation,
  FMX.MultiView,
  FMX.ListView.Adapters.Base,
  FMX.ListView,
  ZonaAPI,
  ZonaAPI.Types,
  FMX.ListView.Types,
  FMX.ListView.Appearances,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.StdCtrls,
  FrameUiManager,
  FMX.Edit,
  FMX.Objects;

type
  TForm2 = class(TForm)
    MultiView1: TMultiView;
    ListBox1: TListBox;
    lbiSerial: TListBoxItem;
    lbiMovie: TListBoxItem;
    lbiFilter: TListBoxItem;
    lyt1: TLayout;
    ToolBar1: TToolBar;
    btnMenu: TButton;
    btnBack: TButton;
    edtSearch: TEdit;
    SearchEditButton1: TSearchEditButton;
    procedure FormCreate(Sender: TObject);
    procedure ListBox1ItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
    procedure FormDestroy(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure SearchEditButton1Click(Sender: TObject);
    procedure lbiMovieClick(Sender: TObject);
    procedure lbiSerialClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FrameStore: TFrameManager;
  end;

var
  Form2: TForm2;

implementation

uses
  ZonaClient.UI.ItemView,
  ZonaClient.UI.MediaList;

{$R *.fmx}

procedure TForm2.btnBackClick(Sender: TObject);
begin
  FrameStore.Back;
  btnBack.Visible := FrameStore.CanBack;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  ListBox1.ItemIndex := 0;
  FrameStore := TFrameManager.Create(Form2.lyt1);
  FrameStore.OnNavigate :=
    procedure(IsRight: Boolean)
    begin
      btnBack.Visible := FrameStore.CanBack;
    end;

  FrameStore.RegFrame<TUiMediaList>('MediaList',
    procedure
    begin
      FrameStore.getFrame<TUiMediaList>().SetFrameStore(FrameStore);
      Form2.MultiView1.HideMaster;
    end);
  lbiSerialClick(Self);
  FrameStore.Navigate('MediaList');
  btnMenu.Visible := True;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  FrameStore.Free;
end;

procedure TForm2.lbiMovieClick(Sender: TObject);
begin
  FrameStore.getFrame<TUiMediaList>().ShowMovie(1)
end;

procedure TForm2.lbiSerialClick(Sender: TObject);
begin
  FrameStore.getFrame<TUiMediaList>().ShowSeries(1);
end;

procedure TForm2.ListBox1ItemClick(const Sender: TCustomListBox; const Item:
  TListBoxItem);
begin
  FrameStore.Navigate('MediaList');
end;

procedure TForm2.SearchEditButton1Click(Sender: TObject);
begin
  FrameStore.getFrame<TUiMediaList>.Search(edtSearch.Text);
  FrameStore.Navigate('MediaList');
end;

end.

