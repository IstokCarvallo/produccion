$PBExportHeader$w_mant_mues_traspasocliente_folio.srw
$PBExportComments$Mantenedor de Inspectores.
forward
global type w_mant_mues_traspasocliente_folio from w_mant_directo
end type
type uo_selcliente from uo_seleccion_clientesprod within w_mant_mues_traspasocliente_folio
end type
type uo_selplanta from uo_seleccion_plantas within w_mant_mues_traspasocliente_folio
end type
type st_1 from statictext within w_mant_mues_traspasocliente_folio
end type
type st_2 from statictext within w_mant_mues_traspasocliente_folio
end type
type uo_selclientenew from uo_seleccion_clientesprod within w_mant_mues_traspasocliente_folio
end type
type st_3 from statictext within w_mant_mues_traspasocliente_folio
end type
type em_folio from editmask within w_mant_mues_traspasocliente_folio
end type
type st_4 from statictext within w_mant_mues_traspasocliente_folio
end type
type cb_recepcion from commandbutton within w_mant_mues_traspasocliente_folio
end type
end forward

global type w_mant_mues_traspasocliente_folio from w_mant_directo
integer width = 3941
integer height = 1972
string title = "MANTENCIÓN ALTURA PALLET"
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
st_1 st_1
st_2 st_2
uo_selclientenew uo_selclientenew
st_3 st_3
em_folio em_folio
st_4 st_4
cb_recepcion cb_recepcion
end type
global w_mant_mues_traspasocliente_folio w_mant_mues_traspasocliente_folio

type variables
uo_traspasocliente						iuo_Traspaso
w_mant_deta_traspasorecepcion	iw_Mantencion
end variables

forward prototypes
public subroutine wf_traspasa ()
end prototypes

public subroutine wf_traspasa ();String	ls_Usuario, ls_Computador
Long	ll_Cliente, ll_Numero, ll_Planta, ll_ClienteNew, ll_Fila

SetPointer(HourGlass!)

ll_Cliente			=	uo_SelCliente.Codigo
ll_Planta			=	uo_SelPlanta.Codigo
ll_ClienteNew	=	uo_SelClienteNew.Codigo
ll_numero		=	dw_1.Object.tras_codigo[1]

DECLARE Traspaso_Cliente PROCEDURE FOR dbo.FProc_TraspasoCliente_RecepcionEliminaFolio	
		  @cliente 		= :ll_Cliente,
		  @planta 		= :ll_Planta,
		  @numero 		= :ll_Numero
Using SQLCA;

EXECUTE Traspaso_Cliente;
			
If Sqlca.SQLCode = -1 Then
	F_ErrorBaseDatos(sqlca, "Lectura SP Traspaso Cliente. Debera Crear Recepcion de Forma Manual.")
Else
	MessageBox('Atencion...', 'Se creo recepcion para Cliente:' + String(ll_ClienteNew, '000') + '~n~nTraspaso Nro: ' + String(ll_Numero, '00000000'), Information!, Ok!)
End If

Close Traspaso_Cliente;

SetPointer(Arrow!)

end subroutine

on w_mant_mues_traspasocliente_folio.create
int iCurrent
call super::create
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
this.st_1=create st_1
this.st_2=create st_2
this.uo_selclientenew=create uo_selclientenew
this.st_3=create st_3
this.em_folio=create em_folio
this.st_4=create st_4
this.cb_recepcion=create cb_recepcion
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.uo_selcliente
this.Control[iCurrent+2]=this.uo_selplanta
this.Control[iCurrent+3]=this.st_1
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.uo_selclientenew
this.Control[iCurrent+6]=this.st_3
this.Control[iCurrent+7]=this.em_folio
this.Control[iCurrent+8]=this.st_4
this.Control[iCurrent+9]=this.cb_recepcion
end on

on w_mant_mues_traspasocliente_folio.destroy
call super::destroy
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.uo_selclientenew)
destroy(this.st_3)
destroy(this.em_folio)
destroy(this.st_4)
destroy(this.cb_recepcion)
end on

event open;call super::open;Boolean	lb_Cerrar = False

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelClienteNew.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	iuo_Traspaso	=	Create uo_traspasocliente
	
	uo_SelCliente.Seleccion(False, False)
	uo_SelClienteNew.Seleccion(False, False)
	uo_SelPlanta.Seleccion(False, False)
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlanta.Filtra(1)
	uo_SelPlanta.Inicia(gi_CodPlanta)
End If
end event

event ue_antesguardar;call super::ue_antesguardar;Long	ll_Fila, ll_Recepcion
String	ls_Fecha

If IsNull(uo_SelClienteNew.Codigo) or uo_SelClienteNew.Codigo = -1 Then 
	MEssageBox('Atencion', 'Debe Ingresar Codigo de cliente Destino', Information!, OK!)
	Message.DoubleParm = -1
	Return
End If

Open(w_sele_observa)

istr_Mant = Message.PowerObjectParm 

ll_Recepcion = iuo_Traspaso.MaximaRecepcion(uo_SelCliente.Codigo, uo_SelPlanta.codigo, SQLCA)

ls_Fecha = String(Today(), 'yyyy/mm/dd hh:mm:ss')

For ll_Fila = 1 To dw_1.RowCount()
	dw_1.Object.tras_codigo[ll_Fila]	=	ll_Recepcion
	dw_1.Object.tras_fectra[ll_Fila]	=	DateTime(ls_Fecha)
	dw_1.Object.tras_clinew[ll_Fila]	=	uo_SelClienteNew.Codigo
	dw_1.Object.usua_codigo[ll_Fila]	=	gstr_Us.Nombre
	dw_1.Object.tras_comput[ll_Fila]	=	gstr_Us.Computador
	dw_1.Object.tras_observ[ll_Fila]	=	istr_Mant.Argumento[1]
Next
end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db() THEN
	wf_Traspasa()
	w_main.SetMicroHelp("Información Grabada.")
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF

Close(This)
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_traspasocliente_folio
integer x = 73
integer y = 44
integer width = 3310
integer height = 364
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_traspasocliente_folio
boolean visible = false
integer x = 3520
integer y = 336
integer taborder = 60
boolean enabled = false
end type

event pb_nuevo::clicked;call super::clicked;pb_lectura.Enabled	= True
pb_insertar.Enabled	= False
pb_eliminar.Enabled	= False
pb_grabar.Enabled		= False
pb_imprimir.Enabled	= False

il_fila					= 0

IF gstr_apl.CodigoSistema <> 23 THEN
	IF gi_codexport = gi_cliebase THEN
	//	IF gi_codplanta <> 2 AND gi_codplanta <> 3 AND gi_codplanta <> 4 THEN
			istr_mant.Solo_Consulta = True
	//	END IF	
	END IF	
END IF	


end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_traspasocliente_folio
boolean visible = false
integer x = 3520
integer y = 64
integer taborder = 50
boolean enabled = false
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_traspasocliente_folio
boolean visible = false
integer x = 3520
integer y = 720
integer taborder = 90
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_traspasocliente_folio
boolean visible = false
integer x = 3520
integer y = 540
integer taborder = 80
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_traspasocliente_folio
integer x = 3520
integer y = 1464
integer taborder = 120
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_traspasocliente_folio
boolean visible = false
integer x = 3520
integer y = 1080
integer taborder = 110
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_traspasocliente_folio
integer x = 3520
integer y = 900
integer taborder = 100
boolean enabled = true
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_traspasocliente_folio
integer x = 73
integer y = 456
integer width = 3310
integer height = 1292
integer taborder = 70
string dataobject = "dw_mues_traspasocliente"
boolean hscrollbar = true
end type

type uo_selcliente from uo_seleccion_clientesprod within w_mant_mues_traspasocliente_folio
integer x = 544
integer y = 80
integer height = 96
integer taborder = 30
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_mant_mues_traspasocliente_folio
integer x = 544
integer y = 240
integer height = 100
integer taborder = 60
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type st_1 from statictext within w_mant_mues_traspasocliente_folio
integer x = 165
integer y = 96
integer width = 306
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
boolean focusrectangle = false
end type

type st_2 from statictext within w_mant_mues_traspasocliente_folio
integer x = 165
integer y = 256
integer width = 306
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
boolean focusrectangle = false
end type

type uo_selclientenew from uo_seleccion_clientesprod within w_mant_mues_traspasocliente_folio
integer x = 1536
integer y = 240
integer height = 96
integer taborder = 40
boolean bringtotop = true
end type

on uo_selclientenew.destroy
call uo_seleccion_clientesprod::destroy
end on

type st_3 from statictext within w_mant_mues_traspasocliente_folio
integer x = 1536
integer y = 96
integer width = 585
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
string text = "Cliente a Traspasar"
boolean focusrectangle = false
end type

type em_folio from editmask within w_mant_mues_traspasocliente_folio
integer x = 2661
integer y = 164
integer width = 558
integer height = 96
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean italic = true
boolean border = false
alignment alignment = center!
string mask = "#######"
end type

event modified;Long	ll_New, ll_Busca
String	ls_Busca, ls_Fecha

If IsNull(This.Text) Or This.Text = '' Then Return

ls_Busca = 'paen_numero = ' + This.Text

ll_Busca = dw_1.Find(ls_Busca, 1, dw_1.RowCount(), Primary!)

If ll_Busca = 0 Then
	If iuo_Traspaso.ExisteFolio(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, Long(This.Text), True, SQLCA) Then
		
		If dw_1.RowCount() > 0 Then
			If (Date(iuo_Traspaso.FechaRecepcion) <> Date(dw_1.Object.rfpe_fecrec[1])) Then
				MessageBox('ERROR', 'Folios con distinta fehca de recepcion', StopSign!, OK!)
				This.Text = ''
				Return
			End If
		ENd If
		
		ll_New = dw_1.InsertRow(0)
		
		ls_Fecha = String(iuo_Traspaso.FechaRecepcion, 'yyyy/mm/dd')
		
		dw_1.Object.clie_codigo[ll_New]		= uo_SelCliente.Codigo
		dw_1.Object.plde_codigo[ll_New]		= uo_SelPlanta.Codigo
		dw_1.Object.rfpe_numero[ll_New]	= iuo_Traspaso.Recepcion
		dw_1.Object.rfpe_fecrec[ll_New]		= Date(ls_Fecha)
		dw_1.Object.rfpe_nrores[ll_New] 		= iuo_Traspaso.Guia
		dw_1.Object.paen_numero[ll_New] 	= iuo_Traspaso.Folio
		
		dw_1.SelectRow(0, False)
		dw_1.SelectRow(ll_New, True)
		dw_1.SetRow(ll_New)
		
		This.Text = ''
	End If
Else
	MessageBox('Atencion', 'Folio Pallet (' + This.Text + '),  ya se encuentran cargado:', Exclamation!, OK!)
	This.Text = ''
	Return
End If
end event

type st_4 from statictext within w_mant_mues_traspasocliente_folio
integer x = 2661
integer y = 96
integer width = 585
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
string text = "Folio a Traspasar"
boolean focusrectangle = false
end type

type cb_recepcion from commandbutton within w_mant_mues_traspasocliente_folio
integer x = 2661
integer y = 292
integer width = 558
integer height = 92
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean italic = true
string pointer = "AppStarting!"
string text = "Por Recepcion"
boolean flatstyle = true
end type

event clicked;istr_Mant.Argumento[1] = String(uo_SelCliente.Codigo)
istr_Mant.Argumento[2] = String(uo_SelPlanta.Codigo)
istr_Mant.dw	    			= dw_1

 istr_Mant.Agrega			=	True

OpenWithParm(iw_mantencion, istr_Mant)
end event

