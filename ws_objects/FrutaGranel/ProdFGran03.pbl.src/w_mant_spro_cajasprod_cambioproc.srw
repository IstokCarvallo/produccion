$PBExportHeader$w_mant_spro_cajasprod_cambioproc.srw
forward
global type w_mant_spro_cajasprod_cambioproc from w_mant_directo
end type
type st_1 from statictext within w_mant_spro_cajasprod_cambioproc
end type
type st_2 from statictext within w_mant_spro_cajasprod_cambioproc
end type
type st_3 from statictext within w_mant_spro_cajasprod_cambioproc
end type
type sle_caja from singlelineedit within w_mant_spro_cajasprod_cambioproc
end type
type uo_selcliente from uo_seleccion_clientesprod within w_mant_spro_cajasprod_cambioproc
end type
type uo_selplanta from uo_seleccion_plantas within w_mant_spro_cajasprod_cambioproc
end type
end forward

global type w_mant_spro_cajasprod_cambioproc from w_mant_directo
integer width = 3246
integer height = 1164
string title = "Mantención de Cajas - Cambio de Proceso"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
event ue_validapassword ( )
st_1 st_1
st_2 st_2
st_3 st_3
sle_caja sle_caja
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
end type
global w_mant_spro_cajasprod_cambioproc w_mant_spro_cajasprod_cambioproc

type variables
Integer 				ii_productor, ii_proceso, ii_nro
Long					ii_caja
DataWindowChild	idwc_categorias, idwc_Productor
end variables

forward prototypes
public subroutine wf_bloqueacolumnas (boolean ab_bloquea)
public subroutine buscaorden ()
protected function boolean wf_actualiza_db ()
public function boolean validaproceso ()
end prototypes

event ue_validapassword();str_mant	lstr_mant

lstr_mant.Argumento[1]	=	"Granel - Genera Cuadratura Proceso"
lstr_mant.Argumento[2]	=	gstr_paramplanta.PassPack

OpenWithParm(w_password, lstr_mant)

lstr_mant	=	Message.PowerObjectParm

IF lstr_mant.Respuesta = 0 THEN Close(This)
end event

public subroutine wf_bloqueacolumnas (boolean ab_bloquea);//
end subroutine

public subroutine buscaorden ();Str_Busqueda				lstr_busq
uo_spro_ordenproceso 	luo_ordenproceso
String 						ls_Nula
Integer						li_capr_tipdoc

luo_ordenproceso		=	Create uo_spro_ordenproceso

SetNull(ls_nula)

li_capr_tipdoc			=	dw_1.Object.capr_tipdoc[dw_1.GetRow()]
lstr_busq.argum[1]	=	String(uo_SelPlanta.Codigo)
lstr_busq.argum[2]	=	"0"
lstr_busq.argum[3]	=	String(li_capr_tipdoc)
lstr_busq.argum[4]	=  String(uo_SelCliente.Codigo)

OpenWithParm(w_busc_spro_ordenproceso, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_busq.argum[1] <> "" and lstr_busq.argum[1]<> "0" THEN
	
	IF luo_ordenproceso.Existe(uo_SelPlanta.Codigo, li_capr_tipdoc, Integer(lstr_busq.argum[6]),True,Sqlca, uo_SelCliente.Codigo) THEN	
		IF ( luo_ordenproceso.VarRot = dw_1.Object.capr_varrot[1] ) OR &
			   ( ( luo_ordenproceso.VarRot > 50 ) AND &
				  ( dw_1.Object.capr_varrot[1] > 50) ) OR dw_1.Object.capr_tipdoc[dw_1.GetRow()] = 5 THEN
				  
			istr_mant.argumento[3]	= lstr_busq.argum[6]
			dw_1.SetItem(1,"capr_docrel",long(lstr_busq.argum[6]))
			
			dw_1.object.prod_codigo[1] = luo_ordenproceso.productor
			dw_1.Object.vari_codigo[1] = luo_ordenproceso.Variedad
		ELSE
			dw_1.SetItem(1,"capr_docrel",long(ls_Nula))
		END IF
	ELSE
		dw_1.SetItem(1,"capr_docrel",long(ls_Nula))
	END IF
END IF
end subroutine

protected function boolean wf_actualiza_db ();boolean			lb_Retorno
Integer			li_especie

dw_1.AcceptText()

ii_productor	=	dw_1.Object.prod_codigo[1]
ii_proceso		=	dw_1.Object.capr_docrel[1]
li_especie		=	dw_1.Object.espe_codigo[1]

If dw_1.ModIfiedCount() > 0 Then
	DECLARE ModIficaCajas PROCEDURE FOR dbo.fgran_modIfica_cajasprod
		@Planta 		=	:uo_SelPlanta.Codigo,   
		@Cliente 	=	:uo_SelCliente.Codigo,   
		@Numero 	=	:ii_caja,   
		@Proceso	=	:ii_proceso,   
		@Productor 	=	:ii_productor,
		@Especie	=	:li_especie
	Using sqlca;
			
	Execute ModIficaCajas;

	If sqlca.SQLCode <> 100 Then
		Rollback;
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla CajasProd")
		lb_Retorno	=	False
		dw_1.Reset()
		sle_caja.Text = ""
		sle_caja.SetFocus()
	Else
		Commit;
		dw_1.Reset()
		sle_caja.Text = ""
		sle_caja.SetFocus()
	End If
End If

Return TRUE

end function

public function boolean validaproceso ();Integer					li_capr_tipdoc

uo_spro_ordenproceso	luo_op
luo_op				=	Create uo_spro_ordenproceso

li_capr_tipdoc		=	dw_1.Object.capr_tipdoc[dw_1.GetRow()]

luo_op.Existe(uo_SelPlanta.Codigo, li_capr_tipdoc, ii_nro, TRUE, SQLCA, uo_SelCliente.Codigo)
li_capr_tipdoc	=	dw_1.Object.capr_tipdoc[dw_1.GetRow()]

IF luo_op.Estado = 5 THEN
	MessageBox("Protección Integridad de datos", "imposible modificar caja, pues pertenece ~r~n"+&
				  "a un proceso que esta con Cierre Web. Ingrese Otra Caja", StopSign!)
	Return False
END IF
Return True
end function

on w_mant_spro_cajasprod_cambioproc.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.st_3=create st_3
this.sle_caja=create sle_caja
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.st_3
this.Control[iCurrent+4]=this.sle_caja
this.Control[iCurrent+5]=this.uo_selcliente
this.Control[iCurrent+6]=this.uo_selplanta
end on

on w_mant_spro_cajasprod_cambioproc.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.sle_caja)
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
end on

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta

DO
	ll_fila	= dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, ii_caja)
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SetFocus()
		pb_grabar.Enabled 	= 	TRUE
		ii_nro					=	dw_1.Object.capr_docrel[1]
		
		dw_1.GetChild("prod_codigo", idwc_Productor)
		idwc_Productor.SetTransObject(SQLCA)
		idwc_Productor.Retrieve(-1)
			
		dw_1.GetChild("cate_codigo", idwc_categorias)
		idwc_categorias.SetTransObject(SQLCA)
		idwc_categorias.Retrieve()
		
		idwc_categorias.SetFilter("cate_embala = 1")
		idwc_categorias.Filter()
		
		IF NOT ValidaProceso() THEN
			dw_1.Reset()
			
			pb_grabar.Enabled 	= 	FALSE
			sle_caja.Text = ""
			sle_caja.SetFocus()
		END IF
				
	ELSE
		MessageBox("Error", "La caja ingresada no existe")
		pb_grabar.Enabled 	= 	FALSE
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event open;Boolean	lb_Cerrar

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True

im_menu		=	m_principal

IF gs_Ambiente = "Windows" THEN
	This.ParentWindow().ToolBarVisible	=	True
	im_menu.Item[1].Item[6].Enabled		=	True
	im_menu.Item[7].Visible					=	False
END IF

This.Icon									=	Gstr_apl.Icono

dw_1.SetTransObject(sqlca)

istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()
istr_mant.Solo_Consulta			=	istr_mant.UsuarioSoloConsulta

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)

If lb_Cerrar Then
	Close(This)
Else
	uo_SelPlanta.Seleccion(False, False)
	uo_SelCliente.Seleccion(False, False)
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlanta.Inicia(gstr_ParamPlanta.CodigoPlanta)
	
	IF NOT IsNull(gstr_paramplanta.PassPack) AND Trim(gstr_paramplanta.PassPack) <> '' THEN
		PostEvent("ue_validapassword")
	END IF
End If
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_spro_cajasprod_cambioproc
integer x = 37
integer width = 2647
integer height = 268
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_spro_cajasprod_cambioproc
boolean visible = false
integer x = 3141
integer y = 456
integer taborder = 80
boolean enabled = false
end type

type pb_lectura from w_mant_directo`pb_lectura within w_mant_spro_cajasprod_cambioproc
integer x = 2775
integer y = 76
integer taborder = 20
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_spro_cajasprod_cambioproc
boolean visible = false
integer x = 3168
integer y = 872
integer taborder = 100
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_spro_cajasprod_cambioproc
boolean visible = false
integer x = 3154
integer y = 620
integer taborder = 90
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_spro_cajasprod_cambioproc
integer x = 2757
integer y = 736
integer taborder = 50
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_spro_cajasprod_cambioproc
boolean visible = false
integer x = 2930
integer y = 1176
integer taborder = 110
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_spro_cajasprod_cambioproc
integer x = 2757
integer y = 452
integer taborder = 40
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_spro_cajasprod_cambioproc
integer x = 41
integer y = 364
integer width = 2647
integer height = 684
boolean titlebar = true
string title = "Datos Originales Caja"
string dataobject = "dw_mant_mues_spro_cajasprod_proceso"
boolean vscrollbar = false
boolean livescroll = false
end type

event dw_1::buttonclicked;call super::buttonclicked;string ls_columna

ls_columna = dwo.Name

CHOOSE CASE ls_columna
		
	CASE "b_procesos"
		
		buscaorden()
		
END CHOOSE		
end event

event dw_1::itemchanged;call super::itemchanged;Integer						li_Null, li_cliente, li_capr_tipdoc
String						ls_Columna
Str_Busqueda				lstr_busq
String 						ls_Nula
uo_spro_ordenproceso 	luo_ordenproceso

dw_1.accepttext()
SetNull(li_Null)

luo_ordenproceso		=	Create uo_spro_ordenproceso

ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna
	CASE "capr_docrel"
		li_capr_tipdoc	=	This.Object.capr_tipdoc[Row]
		
		IF luo_ordenproceso.Existe(uo_SelPlanta.Codigo, li_capr_tipdoc, Integer(data),True,Sqlca, uo_SelCliente.Codigo) THEN
			IF luo_ordenproceso.Estado > 3 THEN
				 IF luo_ordenproceso.Estado = 4 THEN
					MessageBox("Atención","Procesos Digitado se encuentra Cerrado, Favor digite un Proceso Abierto.")
				 ELSE
					MessageBox("Atención","Procesos Con Cierre Web, Favor digite un Proceso Abierto.")
				 END IF
				 pb_lectura.triggerEvent(Clicked!)
				 Return 1
			ELSE
				IF ( luo_ordenproceso.VarRot = dw_1.Object.capr_varrot[1] ) OR &
					( ( luo_ordenproceso.VarRot > 50 ) AND &
					  ( dw_1.Object.capr_varrot[1] > 50) ) OR This.Object.capr_tipdoc[Row] = 5 THEN
				
					dw_1.object.prod_codigo[1] = luo_ordenproceso.productor
					dw_1.Object.vari_codigo[1] = luo_ordenproceso.variedad
					ii_nro	=	Integer(Data)
				ELSE
					dw_1.SetItem(1,"capr_docrel",ii_nro)
					MessageBox("Error","Los Procesos no tienen variedad rotulada compatible.")
					Return 1
				END IF
			END IF
		ELSE
			dw_1.SetItem(1,"capr_docrel",ii_nro)
			MessageBox("Error","No existe la orden de proceso ingresada")
			Return 1
		END IF
	
END CHOOSE
end event

event dw_1::itemerror;call super::itemerror;Return 1
end event

type st_1 from statictext within w_mant_spro_cajasprod_cambioproc
integer x = 183
integer y = 132
integer width = 311
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type st_2 from statictext within w_mant_spro_cajasprod_cambioproc
integer x = 183
integer y = 216
integer width = 311
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type st_3 from statictext within w_mant_spro_cajasprod_cambioproc
integer x = 1669
integer y = 128
integer width = 297
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Nro. Caja"
boolean focusrectangle = false
end type

type sle_caja from singlelineedit within w_mant_spro_cajasprod_cambioproc
integer x = 2016
integer y = 120
integer width = 466
integer height = 84
integer taborder = 10
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

event modified;ii_caja	=	long(sle_caja.Text)
end event

type uo_selcliente from uo_seleccion_clientesprod within w_mant_spro_cajasprod_cambioproc
event destroy ( )
integer x = 530
integer y = 120
integer height = 88
integer taborder = 70
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_mant_spro_cajasprod_cambioproc
event destroy ( )
integer x = 530
integer y = 208
integer height = 88
integer taborder = 30
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

