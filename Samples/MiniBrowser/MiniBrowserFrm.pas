unit MiniBrowserFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, CVcl.WebView, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm51 = class(TForm)
    Panel1: TPanel;
    URLEdit: TEdit;
    Go: TButton;
    BackButton: TButton;
    ForwButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure GoClick(Sender: TObject);
    procedure BackButtonClick(Sender: TObject);
    procedure ForwButtonClick(Sender: TObject);
  private
    { Private declarations }
    FWebView: TWebView;
    procedure FinishLoading(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form51: TForm51;

implementation

{$R *.dfm}

procedure TForm51.FormCreate(Sender: TObject);
begin
  FWebView := TWebView.Create(Self);
  FWebView.Parent := Self;
  FWebView.Align := alClient;
  FWebView.OnFinishLoading := FinishLoading;
end;

procedure TForm51.BackButtonClick(Sender: TObject);
begin
  FWebView.GoBack;
end;

procedure TForm51.FinishLoading(Sender: TObject);
begin
  ShowMessage('Finish loading');
end;

procedure TForm51.ForwButtonClick(Sender: TObject);
begin
  FWebView.GoForward;
end;

procedure TForm51.GoClick(Sender: TObject);
begin
  FWebView.Navigate(URLEdit.Text);
end;

end.
