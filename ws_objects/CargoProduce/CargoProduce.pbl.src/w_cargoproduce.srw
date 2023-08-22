$PBExportHeader$w_cargoproduce.srw
forward
global type w_cargoproduce from w_systray
end type
type mle_texto from multilineedit within w_cargoproduce
end type
type dw_1 from uo_dw within w_cargoproduce
end type
type dw_2 from uo_dw within w_cargoproduce
end type
end forward

global type w_cargoproduce from w_systray
integer width = 3442
integer height = 1420
string title = "Carga CargoProduce"
boolean minbox = true
boolean maxbox = true
boolean resizable = true
mle_texto mle_texto
dw_1 dw_1
dw_2 dw_2
end type
global w_cargoproduce w_cargoproduce

type variables

end variables

forward prototypes
public function boolean wf_carga ()
public function boolean wf_archivo (string mensaje)
public subroutine wf_inserta (string mensaje)
public subroutine wf_actualiza (string embarque, integer fila)
protected function boolean wf_actualiza_db ()
end prototypes

public function boolean wf_carga ();uo_ws_cargoproduce	luo_WS
Boolean	lb_Retorno = True
Long		ll_Fila, ll_Respuesta 
String		ls_respuesta

SetPointer(HourGlass!)

luo_WS 				=	Create uo_ws_cargoproduce

wf_Inserta('Inicio de Proceso de Carga...')
wf_Inserta('Inicia recuperacion de instructivos a cargar...')

ll_Fila	= dw_1.Retrieve()

If ll_fila = -1 Then
	wf_Inserta("No es posible conectar la Base de Datos...")
	lb_Retorno = False
ElseIf ll_fila = 0 Then
	wf_Inserta('No hay instructivos para carga...')
	wf_Inserta("Carga de Instructivos terminada.")
ElseIf ll_fila > 0 Then
	wf_Inserta(String(dw_1.RowCount(), '#,##0') + ': instructivos han sido Recuperados...')
	wf_Inserta('Inicia carga de instructivos...')
	
	For ll_Fila = 1 To dw_1.RowCount()
		wf_Inserta('Carga de instructivo (' + dw_1.Object.embq_codigo[ll_Fila] + "),  Nro. " + &
											String(ll_Fila, '#,##0') + " de " + String(dw_1.RowCount(), '#,##0'))
		ll_Respuesta = luo_WS.of_Carga(dw_1.Object.embq_codigo[ll_Fila], ls_respuesta)
		
		If IsNull(ls_respuesta) Then ls_respuesta = ''
		
		If ll_Respuesta = -1 Then
			wf_Inserta('Carga de Instructivo (' + dw_1.Object.embq_codigo[ll_Fila] + "), Fallo en la carga." + ls_respuesta) 
		Else
			wf_Inserta('Carga de Instructivo (' + dw_1.Object.embq_codigo[ll_Fila] + ", Cargado correctamente.")
			wf_Actualiza(dw_1.Object.embq_codigo[ll_Fila], ll_Fila)
		End If	
		
		Yield()
	Next
	
	wf_Inserta("Carga de Instructivos terminada.")
	
	wf_Archivo(mle_Texto.Text)

End If

Destroy luo_WS
SetPointer(Arrow!) 

Return lb_Retorno


end function

public function boolean wf_archivo (string mensaje);Boolean		lb_Retorno = True
Integer		li_Fila
String			ls_Archivo
DataStore	lds_Archivo

lds_Archivo = Create DataStore
lds_Archivo.DataObject = 'dw_Archivo'

If IsNull(Mensaje) Then Mensaje = ''

li_Fila = lds_Archivo.InsertRow(0)

lds_Archivo.Object.Mensaje[li_Fila] = Mensaje

ls_Archivo = 'LOG_' + String(Today(), 'ddmmyyyyThhmm')+ '.TXT'

If lds_Archivo.SaveAs(ls_Archivo, Text!, False) = -1 Then
//	MessageBox('Error', 'No se pùdo generar archivo ('+ ls_Archivo +') con informción solicitda.' , StopSign!, OK! )
	lb_Retorno = False
Else
//	MessageBox('Atencion', 'Archivo ('+ ls_Archivo +') generado satisfactoriamente.' , Information!, OK! )
End If

Destroy lds_Archivo

Return lb_Retorno
end function

public subroutine wf_inserta (string mensaje);If IsNull(Mensaje) Then Mensaje = ''

mle_Texto.Text += String(Today(), 'dd/mm/yyyy hh:mm:ss') + ' - ' + Mensaje + '~r~n'
mle_Texto.Scroll(mle_Texto.LineCount())

end subroutine

public subroutine wf_actualiza (string embarque, integer fila);Long	ll_Fila, ll_New, ll_Busca
String	ls_BUsca

ll_Fila = dw_2.Retrieve()

If ll_Fila = -1 THen
	MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
ElseIf ll_Fila >= 0 Then
	ls_Busca = 'embq_codigo = "' + Embarque + '"' 
	ll_Busca = dw_2.Find(ls_Busca, 1, dw_2.RowCount())
	
	If ll_Busca = 0 Then
		ll_New = dw_2.InsertRow(0)
		dw_2.Object.embq_codigo[ll_New]	= Embarque
		dw_2.Object.logc_cantid[ll_New]		= dw_1.Object.cantidad[Fila]
		dw_2.Object.logc_fechas[ll_New] 		= Today()
	ElseIf ll_Busca > 0 Then
		dw_2.Object.logc_cantid[ll_Busca]		= dw_1.Object.cantidad[Fila]
		dw_2.Object.logc_fechas[ll_Busca] 	= Today()		
	End If	
End If

wf_actualiza_db()
wf_Inserta("Actualiza total de Instructivos en LOG de Carga.")

Return
end subroutine

protected function boolean wf_actualiza_db ();Boolean	lb_AutoCommit, lb_Retorno
DateTime	ldt_FechaHora

ldt_FechaHora		=	F_FechaHora()
dw_2.GrupoFecha	=	ldt_FechaHora

If Not dw_2.uf_check_required(0) Then Return False
If Not dw_2.uf_validate(0) Then Return False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

If dw_2.Update(True, False) = 1 Then 
	Commit;
	
	If sqlca.SQLCode <> 0 Then
		F_ErrorBaseDatos(sqlca, This.Title)
		lb_Retorno	=	False
	Else
		lb_Retorno	=	True
		dw_2.ResetUpdate()
	End If
Else
	RollBack;
	
	If sqlca.SQLCode <> 0 Then F_ErrorBaseDatos(sqlca, This.Title)
	lb_Retorno	=	False
End If

sqlca.AutoCommit	=	lb_AutoCommit

Return lb_Retorno
end function

on w_cargoproduce.create
int iCurrent
call super::create
this.mle_texto=create mle_texto
this.dw_1=create dw_1
this.dw_2=create dw_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.mle_texto
this.Control[iCurrent+2]=this.dw_1
this.Control[iCurrent+3]=this.dw_2
end on

on w_cargoproduce.destroy
call super::destroy
destroy(this.mle_texto)
destroy(this.dw_1)
destroy(this.dw_2)
end on

event open;call super::open;in_tray.of_delete_icon(This, True)

dw_1.SetTransObject(SQLCA)
dw_2.SetTransObject(SQLCA)

mle_Texto.Setfocus()
mle_Texto.Text = ''

If Not wf_Carga() Then wf_Inserta('Fallo en el proceso de carga...')

//Close(This)

//Timer(3)
end event

event timer;//
//Timer(0)
//
//
//
//Timer(3)
end event

event resize;call super::resize;
mle_texto.x = 40
mle_texto.y = 30
mle_texto.Width = pb_salir.x - mle_texto.x - 25
mle_texto.Height= pb_salir.y + mle_texto.y + 165
end event

type pb_salir from w_systray`pb_salir within w_cargoproduce
integer x = 3013
integer y = 1000
integer height = 244
string powertiptext = "Salir"
end type

type mle_texto from multilineedit within w_cargoproduce
integer x = 41
integer y = 24
integer width = 2935
integer height = 1212
integer taborder = 10
boolean bringtotop = true
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

type dw_1 from uo_dw within w_cargoproduce
boolean visible = false
integer x = 3031
integer y = 88
integer width = 233
integer height = 172
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_carga"
boolean vscrollbar = false
end type

type dw_2 from uo_dw within w_cargoproduce
boolean visible = false
integer x = 3031
integer y = 276
integer width = 233
integer height = 172
integer taborder = 21
boolean bringtotop = true
string dataobject = "dw_log"
boolean vscrollbar = false
end type

event sqlpreview;//
end event

