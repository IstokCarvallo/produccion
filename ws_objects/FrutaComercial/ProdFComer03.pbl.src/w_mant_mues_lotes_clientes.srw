$PBExportHeader$w_mant_mues_lotes_clientes.srw
forward
global type w_mant_mues_lotes_clientes from w_mant_directo
end type
type st_1 from statictext within w_mant_mues_lotes_clientes
end type
type dw_planta from datawindow within w_mant_mues_lotes_clientes
end type
type st_2 from statictext within w_mant_mues_lotes_clientes
end type
type em_orden from editmask within w_mant_mues_lotes_clientes
end type
type cb_orden from commandbutton within w_mant_mues_lotes_clientes
end type
type cb_todos from commandbutton within w_mant_mues_lotes_clientes
end type
type st_4 from statictext within w_mant_mues_lotes_clientes
end type
type sle_nombre from singlelineedit within w_mant_mues_lotes_clientes
end type
end forward

global type w_mant_mues_lotes_clientes from w_mant_directo
string tag = "Ventana de generación de Movimiento de Recepción Clientes"
integer width = 3374
string title = "Recepción Comercial Clientes"
boolean controlmenu = false
st_1 st_1
dw_planta dw_planta
st_2 st_2
em_orden em_orden
cb_orden cb_orden
cb_todos cb_todos
st_4 st_4
sle_nombre sle_nombre
end type
global w_mant_mues_lotes_clientes w_mant_mues_lotes_clientes

type variables
datawindowchild idwc_planta

String   is_rutcli
//Integer  ii_codcli
Long     il_codcli
end variables

forward prototypes
public subroutine habilitaencab (boolean habilita)
protected function boolean wf_actualiza_db ()
public function boolean existeorden (long al_orden, integer ai_tipo)
end prototypes

public subroutine habilitaencab (boolean habilita);

IF habilita THEN
	
	em_orden.enabled = TRUE
	cb_orden.visible = TRUE
	
ELSE
	
	em_orden.enabled = FALSE
	cb_orden.visible = FALSE
	
END IF	
end subroutine

protected function boolean wf_actualiza_db ();
Boolean	lb_AutoCommit, lb_Retorno
Integer li_planta, li_tipomovto
Long    ll_orden

IF Not dw_1.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

li_planta = dw_planta.Object.plde_codigo[1]
li_tipomovto = 35
ll_orden     =  Long(em_orden.text)

IF dw_1.Update(True, False) = 1 then 
	
	DECLARE GeneraRecepcion PROCEDURE FOR dba.FComer_Genera_lotes_Cliente
								@Planta		 =	:li_Planta,   
								@TipoMovto	 =	:li_TipoMovto,
								@ClienteProd = :il_codcli,
								@OrdenVta    = :ll_orden;
								
	EXECUTE GeneraRecepcion ;
		
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
		RollBack ;
	END IF
	
	Commit;
	
	IF sqlca.SQLCode <> 0 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
		lb_Retorno	=	False
	ELSE
		lb_Retorno	=	True
			
		dw_1.ResetUpdate()
		
		TriggerEvent("ue_recuperadatos")
	END IF
ELSE
	RollBack;
	
	IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
	
	lb_Retorno	=	False
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public function boolean existeorden (long al_orden, integer ai_tipo);Integer	li_planta, li_estado
Long		ll_Numero
String   ls_rut, ls_nombre
Boolean	lb_Retorno = True

sle_nombre.text	=	""
is_rutcli 			=	""

li_planta = dw_planta.Object.plde_codigo[1]

SELECT	odfc_numero, clpr_rut, odfc_estado
	INTO	:ll_Numero, :ls_rut, :li_estado
	FROM	dba.spro_ordenventacomenca 
  WHERE	plde_codigo	=	:li_Planta
	 AND	odfc_numero	=	:al_orden;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Orden de Venta")
	
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode = 100 THEN
	messagebox("Falta de Datos","La Orden de Venta Ingresada No Existe En la Base de Datos. Ingrese o Seleccione Otra.")
	Return False
	lb_Retorno	=	False
ELSE
	
	IF li_estado = 0 THEN
		messagebox("Atención","La Orden de Venta Ingresada se encuentra Cerrada. Ingrese o Seleccione Otra.")
		Return False
	END IF	
	
	//em_rut.text=ls_rut
	is_rutcli = ls_rut
	
	SELECT clpr_nombre, prod_codigo INTO :ls_Nombre, :il_codcli
	 FROM  dba.clienprove
	 WHERE clpr_rut = :ls_rut;
	 
	 IF sqlca.SQLCode = -1 THEN
		 F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Clientes")
		 
	 ELSEIF sqlca.SQLCode = 0 THEN
		
		sle_nombre.text= ls_nombre
		
	 END IF	
	 
	 IF isnull(il_codcli) or il_codcli=0 THEN
		messagebox("Falta de Datos","El Cliente de la Orden de Venta Elegida no posee un Código de Productor Asociado.~rIngrese o Seleccione Otra Orden")
		Return False
	 END IF	
	 
END IF

RETURN lb_Retorno
end function

on w_mant_mues_lotes_clientes.create
int iCurrent
call super::create
this.st_1=create st_1
this.dw_planta=create dw_planta
this.st_2=create st_2
this.em_orden=create em_orden
this.cb_orden=create cb_orden
this.cb_todos=create cb_todos
this.st_4=create st_4
this.sle_nombre=create sle_nombre
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.dw_planta
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.em_orden
this.Control[iCurrent+5]=this.cb_orden
this.Control[iCurrent+6]=this.cb_todos
this.Control[iCurrent+7]=this.st_4
this.Control[iCurrent+8]=this.sle_nombre
end on

on w_mant_mues_lotes_clientes.destroy
call super::destroy
destroy(this.st_1)
destroy(this.dw_planta)
destroy(this.st_2)
destroy(this.em_orden)
destroy(this.cb_orden)
destroy(this.cb_todos)
destroy(this.st_4)
destroy(this.sle_nombre)
end on

event open;x=0
y=0

This.Height	= 1993
im_menu	= m_principal

This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	False
This.Icon									=	Gstr_apl.Icono

dw_planta.GetChild("plde_codigo",idwc_planta)
idwc_planta.SetTransObject(SQLCA)
idwc_planta.retrieve()
dw_planta.SetTransObject(sqlca)
dw_planta.InsertRow(0)

dw_planta.SetItem(1,"plde_codigo",gstr_paramplanta.codigoplanta)
dw_planta.Object.plde_codigo.BackGround.Color = RGB(192,192,192)

dw_1.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()
istr_mant.Solo_Consulta			=	istr_mant.UsuarioSoloConsulta

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)
								
buscar			= "Número:Nmfco_numero,Guía Despacho:Nmfco_guisii"
ordenar			= "Número:mfco_numero,Fecha Movto.:mfco_fecmov,Guía Despacho:mfco_guisii"
is_ultimacol	= "mfco_estret"
end event

event ue_recuperadatos();Long		ll_Fila, Respuesta, ll_orden
Integer  li_planta,li_tipom, li_estado

li_planta = dw_planta.Object.plde_codigo[1]
li_tipom	 = 35
ll_orden  = long(em_orden.text)
li_estado = 1

DO
	ll_Fila	= dw_1.Retrieve(li_planta,li_tipom,li_estado,ll_orden)
	IF ll_Fila = -1 THEN
		Respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_Fila > 0 THEN

			
		HabilitaEncab(False)

		dw_1.SetRow(1)
		dw_1.SetFocus()

		pb_grabar.Enabled		= True
		pb_imprimir.Enabled	= True

	END IF

LOOP WHILE Respuesta = 1

IF Respuesta = 2 THEN Close(This)
end event

event ue_antesguardar();call super::ue_antesguardar;Long ll_fila

FOR ll_fila=1 To dw_1.RowCount()
	IF dw_1.Object.acepta[ll_fila] = 1 THEN dw_1.Object.mfco_estret[ll_fila] = 2
NEXT	
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_lotes_clientes
integer x = 105
integer y = 56
integer width = 2775
integer height = 384
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_lotes_clientes
integer x = 3054
integer y = 428
end type

event pb_nuevo::clicked;call super::clicked;HabilitaEncab(TRUE)

em_orden.Setfocus()
end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_lotes_clientes
integer x = 3054
integer y = 132
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_lotes_clientes
boolean visible = false
integer x = 3054
integer y = 788
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_lotes_clientes
boolean visible = false
integer x = 3054
integer y = 608
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_lotes_clientes
integer x = 3054
integer y = 1532
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_lotes_clientes
boolean visible = false
integer x = 3054
integer y = 1148
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_lotes_clientes
integer x = 3054
integer y = 968
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_lotes_clientes
integer x = 91
integer y = 616
integer width = 2793
integer height = 1140
string dataobject = "dw_mues_movimientos_lotes_clientes"
end type

type st_1 from statictext within w_mant_mues_lotes_clientes
integer x = 247
integer y = 172
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Planta"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_mant_mues_lotes_clientes
integer x = 494
integer y = 160
integer width = 992
integer height = 100
integer taborder = 20
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_planta"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type st_2 from statictext within w_mant_mues_lotes_clientes
integer x = 1614
integer y = 176
integer width = 379
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Orden Venta"
boolean focusrectangle = false
end type

type em_orden from editmask within w_mant_mues_lotes_clientes
integer x = 2021
integer y = 160
integer width = 402
integer height = 100
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;IF Not existeorden(long(this.text),7) THEN
	em_orden.text=""
	RETURN
END IF

end event

type cb_orden from commandbutton within w_mant_mues_lotes_clientes
integer x = 2464
integer y = 168
integer width = 96
integer height = 88
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "..."
end type

event clicked;Long           ll_Nula
Str_Busqueda	lstr_busq

SetNull(ll_Nula)

lstr_busq.argum[1] = String(gstr_param.plde_codigo)
lstr_busq.argum[2] = '2'
lstr_busq.argum[3] = ''									// 


OpenWithParm(w_busc_ordenventacomercial, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_busq.argum[2] <> "" THEN
	
	IF Not existeorden(long(lstr_busq.argum[2]),integer(lstr_busq.argum[1])) THEN
		em_orden.text=""
		RETURN
	ELSE
		em_orden.text=lstr_busq.argum[2]

	END IF

END IF
end event

type cb_todos from commandbutton within w_mant_mues_lotes_clientes
integer x = 110
integer y = 472
integer width = 471
integer height = 112
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Todos"
end type

event clicked;Long ll_fila
		
IF dw_1.RowCount()>0 THEN
	
	IF cb_todos.Text = "Todos" THEN

		FOR ll_fila=1 To dw_1.RowCount()
			dw_1.Object.acepta[ll_fila] = 1
		NEXT	
		
		cb_todos.Text = "Ninguno"
		
	ELSE
		
		FOR ll_fila=1 To dw_1.RowCount()
			dw_1.Object.acepta[ll_fila] = 0
		NEXT	
		
		cb_todos.Text = "Todos"
	END IF	
END IF		
end event

type st_4 from statictext within w_mant_mues_lotes_clientes
integer x = 242
integer y = 312
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Cliente"
boolean focusrectangle = false
end type

type sle_nombre from singlelineedit within w_mant_mues_lotes_clientes
integer x = 494
integer y = 288
integer width = 1079
integer height = 100
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

