$PBExportHeader$w_mant_mues_anulaguia.srw
$PBExportComments$Anulacion de guia despacho produccion
forward
global type w_mant_mues_anulaguia from w_mant_directo
end type
type uo_selcliente from uo_seleccion_clientesprod within w_mant_mues_anulaguia
end type
type uo_selplanta from uo_seleccion_plantas within w_mant_mues_anulaguia
end type
type dw_2 from uo_dw within w_mant_mues_anulaguia
end type
type em_desde from editmask within w_mant_mues_anulaguia
end type
type em_hasta from editmask within w_mant_mues_anulaguia
end type
type st_1 from statictext within w_mant_mues_anulaguia
end type
type st_2 from statictext within w_mant_mues_anulaguia
end type
type st_3 from statictext within w_mant_mues_anulaguia
end type
type st_4 from statictext within w_mant_mues_anulaguia
end type
end forward

global type w_mant_mues_anulaguia from w_mant_directo
integer width = 3451
integer height = 1980
string title = "ANULACION DE GUIAS DESPACHO SII"
event ue_validapassword ( )
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
dw_2 dw_2
em_desde em_desde
em_hasta em_hasta
st_1 st_1
st_2 st_2
st_3 st_3
st_4 st_4
end type
global w_mant_mues_anulaguia w_mant_mues_anulaguia

type variables
Byte					ib_Modulo
uo_GuiaDespacho	iuo_Guia
uo_Mail				iuo_Mail
end variables

forward prototypes
protected function boolean wf_actualiza_db ()
public subroutine wf_anulaguia ()
end prototypes

event ue_validapassword();istr_mant.Argumento[1]	=	"ANULACION DE GUIAS DESPACHO SII"
istr_mant.Argumento[2]	=	gs_Password

OpenWithParm(w_password, istr_mant)

istr_mant	=	Message.PowerObjectParm

IF istr_mant.Respuesta = 0 THEN Close(This)
end event

protected function boolean wf_actualiza_db ();Boolean	lb_AutoCommit, lb_Retorno
DateTime	ldt_FechaHora

ldt_FechaHora		=	F_FechaHora()
dw_1.GrupoFecha	=	ldt_FechaHora

IF Not dw_1.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

wf_replicacion()

IF dw_1.Update(True, False) = 1 then 
	Commit;
	
	IF sqlca.SQLCode <> 0 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
		lb_Retorno	=	False
	ELSE
		lb_Retorno	=	True
		
		wf_AnulaGuia()	
		dw_1.ResetUpdate()			
	END IF
ELSE
	IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
	
	RollBack;
	
	lb_Retorno	=	False
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public subroutine wf_anulaguia ();Long	ll_Fila, ll_New, li_Correo
String	ls_Cuerpo, ls_Asunto, ls_Error, ls_Modulo

ls_Cuerpo = ''

Choose Case ib_Modulo
	Case 1
		ls_Modulo = 'Fruta Embalada'
	Case 2
		ls_Modulo = 'Envases'
	Case 3
		ls_Modulo = 'Fruta Comercial'
	Case 4
		ls_Modulo = 'Fruta Granel'
End Choose

For ll_Fila = 1 To dw_2.RowCount()
	If dw_2.IsSelected(ll_Fila) Then
		iuo_Guia.of_AnulaGuiaDespacho(2, uo_SelCliente.Codigo, uo_SelPlanta.Codigo, dw_2.Object.defe_guides[ll_Fila], ib_Modulo, SQLCA)
		ls_Cuerpo = ls_Cuerpo + ' / ' + String(dw_2.Object.defe_guides[ll_Fila], '00000000')
	End If	
Next

If IsNull(gstr_parempresa.Correo_Anulacion_DTE) Or gstr_parempresa.Correo_Anulacion_DTE = '' Then
	MessageBox('Error', 'No se pudo enviar correo con informe de anulacion, ya que no existe Correo Configurado.', StopSign!, Ok!)
Else
	ls_Asunto = "Anulacion de Guias Despacho SII"
	ls_Cuerpo  = 'Estimados:~n~n' + 'Se anularon los siguientes Numeros de Guías Desapcho: ' + ls_Cuerpo + '~n~n' + &
				'Del modulo: ' + ls_Modulo +  '~n~n' + &
				'Por el usuario: ' + gstr_Us.Nombre + ' desde el computador: ' + gstr_Us.Computador + ' con fecha: ' + String(Today(), 'dd/mm/yyyy')
	
	iuo_Mail.Of_Send({gstr_parempresa.Correo_Anulacion_DTE}, ls_Asunto, ls_Cuerpo, 2)

End If
end subroutine

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta

DO
	ll_fila	= dw_2.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, DateTime(em_Desde.Text), Datetime(em_Hasta.Text), ib_Modulo)

	If ll_fila = -1 Then
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ElseIf ll_fila > 0 Then
		
		dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo)
		
		dw_1.SetRow(1)
		dw_1.SetFocus()
		il_fila					= 1
		pb_grabar.Enabled		= True
	End If
	
LOOP WHILE respuesta = 1

If respuesta = 2 Then Close(This)
end event

event open;call super::open;Boolean lb_Cerrar = False

If IsNull(uo_SelCliente.Codigo) 	Then lb_Cerrar	=	True
If IsNull(uo_SelPlanta.Codigo) 	Then lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else
	iuo_Guia	=	Create uo_GuiaDespacho
	iuo_Mail	=	Create uo_Mail
	
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(False, False)
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlanta.Inicia(gi_CodPlanta)
	
	em_Desde.Text	=	String(Today(), 'dd/mm/yyyy')
	em_Hasta.Text	=	String(Today(), 'dd/mm/yyyy')
	
	dw_2.SetTransObject(SQLCA)
	
	ib_Modulo	=	Byte(Message.StringParm)
		
	PostEvent("ue_validapassword")
	
	buscar			= "Código Tipo Movto:Ntpmv_codigo,Tipo Movimiento Nombre:Stpmv_nombre,Numero:Ndefe_numero,Guia SII:Ndefe_guides"
	ordenar			= "Código Tipo Movto:tpmv_codigo,Tipo Movimiento Nombre:tpmv_nombre,Numero:defe_numero,Guia SII:defe_guides"
End if
end event

on w_mant_mues_anulaguia.create
int iCurrent
call super::create
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
this.dw_2=create dw_2
this.em_desde=create em_desde
this.em_hasta=create em_hasta
this.st_1=create st_1
this.st_2=create st_2
this.st_3=create st_3
this.st_4=create st_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.uo_selcliente
this.Control[iCurrent+2]=this.uo_selplanta
this.Control[iCurrent+3]=this.dw_2
this.Control[iCurrent+4]=this.em_desde
this.Control[iCurrent+5]=this.em_hasta
this.Control[iCurrent+6]=this.st_1
this.Control[iCurrent+7]=this.st_2
this.Control[iCurrent+8]=this.st_3
this.Control[iCurrent+9]=this.st_4
end on

on w_mant_mues_anulaguia.destroy
call super::destroy
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
destroy(this.dw_2)
destroy(this.em_desde)
destroy(this.em_hasta)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.st_4)
end on

event ue_antesguardar;call super::ue_antesguardar;Long	ll_Fila, ll_New
String	ls_Busca

For ll_Fila = 1 To dw_2.RowCount()
	If dw_2.IsSelected(ll_Fila) Then
		ls_Busca = 'clie_codigo = ' + String(dw_2.Object.clie_codigo[ll_Fila]) + &
						' And plde_codigo = ' + String(dw_2.Object.plde_codigo[ll_Fila]) + &
						' And angu_nrogui = ' + String(dw_2.Object.defe_guides[ll_Fila])
						
		If dw_1.Find(ls_Busca, 1, dw_1.RowCount()) = 0 Then
			ll_New = dw_2.InsertRow(0)
			
			dw_1.Object.clie_codigo[ll_New]		=	uo_SelCliente.Codigo
			dw_1.Object.plde_codigo[ll_New]		=	uo_SelPlanta.Codigo
			dw_1.Object.angu_nrogui[ll_New]		=	dw_2.Object.defe_guides[ll_Fila]
			dw_1.Object.angu_fechaa[ll_New]		=	DateTime(Today(), Now())
			dw_1.Object.angu_numero[ll_New]	=	dw_2.Object.defe_numero[ll_Fila]
			dw_1.Object.tpmv_codigo[ll_New]		=	dw_2.Object.tpmv_codigo[ll_Fila]
			dw_1.Object.embq_codigo[ll_New]	=	dw_2.Object.embq_codigo[ll_Fila]
			dw_1.Object.angu_usuari[ll_New]		=	gstr_Us.Nombre
			dw_1.Object.angu_comput[ll_New]	=	gstr_Us.Computador
			dw_1.Object.angu_modulo[ll_New]	=	ib_Modulo
			dw_1.Object.angu_motivo[ll_New]		=	istr_Mant.Argumento[1]
		End If		
	End If	
Next
end event

event ue_guardar;If dw_1.AcceptText() = -1 Then Return

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

istr_Mant.Argumento[1] = ''

Open(w_sele_observa_nulo)

istr_Mant = Message.PowerObjectParm 
If Message.DoubleParm = -1 Then Return

TriggerEvent("ue_antesguardar")
If Message.DoubleParm = -1 Then Return

If wf_actualiza_db() Then
	w_main.SetMicroHelp("Información Grabada.")
	pb_imprimir.Enabled	= True
Else
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	Return
End If

PostEvent('ue_recuperadatos')


end event

event resize;call super::resize;dw_2.x		=	dw_1.x
dw_2.y		=	dw_1.y
dw_2.Width	=	dw_1.Width
dw_2.Height	=	dw_1.Height
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_anulaguia
integer x = 105
integer y = 64
integer width = 2510
integer height = 464
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_anulaguia
integer x = 2875
integer y = 340
integer taborder = 60
end type

event pb_nuevo::clicked;IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE 0
			CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
				CASE 1
					Message.DoubleParm = 0
					Parent.TriggerEvent("ue_guardar")
					IF message.DoubleParm = -1 THEN RETURN
					
				CASE 3
					RETURN
			END CHOOSE
	END CHOOSE
END IF

//wf_BloqueaColumnas(False)

dw_1.Reset()
dw_2.Reset()

//istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta
end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_anulaguia
integer x = 2875
integer y = 60
integer taborder = 50
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_anulaguia
boolean visible = false
integer x = 2875
integer y = 724
integer taborder = 90
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_anulaguia
boolean visible = false
integer x = 2880
integer y = 544
integer taborder = 80
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_anulaguia
integer x = 2875
integer y = 1432
integer taborder = 120
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_anulaguia
boolean visible = false
integer x = 2875
integer y = 1084
integer taborder = 110
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_anulaguia
integer x = 2875
integer y = 904
integer taborder = 100
integer weight = 400
fontcharset fontcharset = ansi!
string picturename = "\Desarrollo 17\Imagenes\Botones\Nulo.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Nulo-bn.png"
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_anulaguia
integer x = 105
integer y = 556
integer width = 2510
integer height = 1132
integer taborder = 70
string dataobject = "dw_mues_anulaguia"
boolean hscrollbar = true
end type

type uo_selcliente from uo_seleccion_clientesprod within w_mant_mues_anulaguia
event destroy ( )
integer x = 398
integer y = 188
integer height = 100
integer taborder = 40
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_mant_mues_anulaguia
integer x = 1618
integer y = 188
integer height = 100
integer taborder = 50
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type dw_2 from uo_dw within w_mant_mues_anulaguia
integer x = 105
integer y = 556
integer width = 2510
integer height = 1128
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_sele_anulaguia"
end type

event clicked;call super::clicked;If Row > 0 Then
	il_fila = Row
	If This.IsSelected(il_fila) Then 
		This.SelectRow(il_fila, False)
	Else
		This.SetRow(il_fila)
		This.SelectRow(il_fila,True)
	End If
End If
end event

type em_desde from editmask within w_mant_mues_anulaguia
integer x = 791
integer y = 360
integer width = 485
integer height = 104
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type em_hasta from editmask within w_mant_mues_anulaguia
integer x = 1641
integer y = 360
integer width = 485
integer height = 104
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type st_1 from statictext within w_mant_mues_anulaguia
integer x = 174
integer y = 204
integer width = 219
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
string text = "Cliente"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_2 from statictext within w_mant_mues_anulaguia
integer x = 498
integer y = 380
integer width = 219
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
string text = "Desde"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_3 from statictext within w_mant_mues_anulaguia
integer x = 1376
integer y = 204
integer width = 219
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
string text = "Planta"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_4 from statictext within w_mant_mues_anulaguia
integer x = 1367
integer y = 380
integer width = 215
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
string text = "Hasta"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

