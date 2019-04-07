unit WebViewStreamFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, CVcl.WebView, Vcl.StdCtrls,
  Vcl.Imaging.jpeg;

type
  TWebViewForm = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    WebView1: TWebView;
    Splitter1: TSplitter;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WebViewForm: TWebViewForm;

implementation

{$R *.dfm}

procedure TWebViewForm.Button1Click(Sender: TObject);
begin
  WebView1.LoadFromString(Memo1.Text);
end;

procedure TWebViewForm.Button2Click(Sender: TObject);
var
  S: TMemoryStream;
begin
  S := TMemoryStream.Create;
  try
    Memo1.Lines.SaveToStream(S);
    S.Position := 0;

    WebView1.LoadFromStream(S);
  finally
    S.Free;
  end;
end;

procedure TWebViewForm.Button3Click(Sender: TObject);
begin
  ShowMessage(WebView1.GetSelectedText);
end;

procedure TWebViewForm.Button4Click(Sender: TObject);
begin
  ShowMessage(WebView1.RunScript('window.document.title'));
end;

end.
