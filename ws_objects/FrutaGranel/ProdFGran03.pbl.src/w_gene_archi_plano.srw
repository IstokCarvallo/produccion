$PBExportHeader$w_gene_archi_plano.srw
forward
global type w_gene_archi_plano from window
end type
type uo_selcliente from uo_seleccion_clientesprod within w_gene_archi_plano
end type
type uo_selplanta from uo_seleccion_plantas within w_gene_archi_plano
end type
type rb_2 from radiobutton within w_gene_archi_plano
end type
type gb_5 from groupbox within w_gene_archi_plano
end type
type em_cajas from editmask within w_gene_archi_plano
end type
type st_7 from statictext within w_gene_archi_plano
end type
type dw_3 from datawindow within w_gene_archi_plano
end type
type st_2 from statictext within w_gene_archi_plano
end type
type st_1 from statictext within w_gene_archi_plano
end type
type st_6 from statictext within w_gene_archi_plano
end type
type sle_mensa from singlelineedit within w_gene_archi_plano
end type
type pb_salir from picturebutton within w_gene_archi_plano
end type
type pb_grabar from picturebutton within w_gene_archi_plano
end type
type gb_3 from groupbox within w_gene_archi_plano
end type
type st_5 from statictext within w_gene_archi_plano
end type
type rb_1 from radiobutton within w_gene_archi_plano
end type
type em_proceso from editmask within w_gene_archi_plano
end type
type cb_1 from commandbutton within w_gene_archi_plano
end type
type dw_2 from datawindow within w_gene_archi_plano
end type
end forward

global type w_gene_archi_plano from window
integer width = 3653
integer height = 816
boolean titlebar = true
string title = "Generación Archivo CajasProd Por Proceso"
boolean controlmenu = true
boolean resizable = true
long backcolor = 16777215
string icon = "AppIcon!"
event ue_guardar pbm_custom11
event ue_buscar pbm_custom12
event ue_ordenar pbm_custom13
event ue_carga_detalle pbm_custom27
event ue_listo pbm_custom28
event ue_antesguardar pbm_custom75
event ue_seleccion pbm_custom17
event ue_imprimir pbm_custom03
event ue_genera_facturas pbm_custom04
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
rb_2 rb_2
gb_5 gb_5
em_cajas em_cajas
st_7 st_7
dw_3 dw_3
st_2 st_2
st_1 st_1
st_6 st_6
sle_mensa sle_mensa
pb_salir pb_salir
pb_grabar pb_grabar
gb_3 gb_3
st_5 st_5
rb_1 rb_1
em_proceso em_proceso
cb_1 cb_1
dw_2 dw_2
end type
global w_gene_archi_plano w_gene_archi_plano

type variables
Menu	im_menu

str_busqueda           		istr_busq

Boolean						ib_Anulacion  
DataWindowChild			idwc_cliente


end variables

forward prototypes
public function boolean wf_actualiza_db ()
public function boolean validaingreso ()
public function boolean existedocproceso (integer ai_planta, long al_numero)
end prototypes

event ue_guardar;String		ls_Archivo,  ls_Ruta

RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)

ls_Archivo	= '\CajasProd' + String(uo_SelPlanta.Codigo) + String(Integer(em_proceso.text), '00000000') + '.CSV'
dw_2.SaveAs(ls_ruta + ls_archivo, CSV!	 ,false) 
sle_mensa.text	= "Archivo Plano ha Sido Generado, " + ls_ruta + ls_archivo
MessageBox("Generación de Archivo","Se ha generado el Archivo")


end event

event ue_listo;w_main.SetMicroHelp("Listo")
SetPointer(Arrow!)

end event

event ue_seleccion;String ls_nula
Str_busqueda	lstr_busq

IF rb_1.Checked THEN
	dw_2.DataObject = "dw_mues_arch_planos_cajasprod"
	dw_2.SetTransObject(sqlca)
	
	SetNull(ls_nula)
	lstr_busq.argum[1]	=	String(uo_SelPlanta.Codigo)
	lstr_Busq.Argum[15]	=	'4'
	lstr_busq.argum[16]	=	String(uo_SelCliente.Codigo)
	
	OpenWithParm(w_busc_spro_ordenproceso_traspaso, lstr_busq)
	lstr_busq	=	Message.PowerObjectParm
	
	IF lstr_busq.argum[1] <> "" and lstr_busq.argum[1]<> "0" THEN
					IF Not Existedocproceso(uo_SelPlanta.Codigo,Integer(lstr_busq.argum[1])) THEN
						em_proceso.Text	=	ls_Nula
						RETURN 1
					ELSE
						em_proceso.Text			=	lstr_busq.argum[1]
						IF NOT ValidaIngreso() THEN
							em_proceso.Text	=	ls_Nula
							RETURN 1
						END IF
					END IF
	END IF
ELSE
	dw_2.DataObject = "dw_mues_arch_planos_cajasprod_despaemba"
	dw_2.SetTransObject(sqlca)
	
	lstr_busq.argum[1]	= 	String(uo_SelCliente.Codigo)
	lstr_busq.argum[2]	= 	String(uo_SelPlanta.Codigo)
	
	OpenWithParm(w_busc_despafrigoen, lstr_busq)
	
	lstr_busq	       = Message.PowerObjectParm
	
	IF lstr_busq.argum[5] <> "" THEN
		em_proceso.Text			=	lstr_busq.argum[5]
		IF NOT ValidaIngreso() THEN
			em_proceso.Text	=	ls_Nula
			RETURN 1
		END IF
	ELSE
		cb_1.SetFocus()
	END IF
END IF
end event

public function boolean wf_actualiza_db ();RETURN True
end function

public function boolean validaingreso ();Boolean	Respuesta	= 	True
String ls_procdespa = " Despacho "

IF rb_1.Checked THEN ls_procdespa = " Proceso "

sle_mensa.Text	=	''
IF dw_2.retrieve(uo_SelCliente.Codigo,uo_SelPlanta.Codigo,Long(em_Proceso.Text)) > 0 THEN
	pb_grabar.Enabled 	= 	True
	em_cajas.Text			=	String(dw_2.RowCount())				
	sle_mensa.Text		=	em_cajas.Text + ' Cajas recuperadas para' + ls_procdespa +  em_Proceso.Text + '. ¿Generar Archivo Plano?'
ELSE
	Respuesta 				=	False
	pb_grabar.Enabled 	= 	False
	em_cajas.Text			=	String(0)
	sle_mensa.Text		=	'No Existen datos para el' + ls_procdespa + 'ingresado, seleccione o ingrese otro'
END IF

RETURN Respuesta 

end function

public function boolean existedocproceso (integer ai_planta, long al_numero);Long		ll_Numero,ll_Productor
Integer	li_Especie, li_Variedad, li_Cantidad, li_Vigencia, &
         	li_periodo, li_linea, li_turno
String   	ls_Nombre, ls_frio
Boolean	lb_Retorno = True

SELECT	op.orpr_numero, 
			op.prod_codigo, 
			op.espe_codigo, 
			op.vari_codigo, 
			op.orpr_canbul, 
			op.orpr_estado, 
			op.frio_tipofr, 
			op.pefr_codigo, 
			op.orpr_nrotur, 
			op.line_codigo, 
			va.vari_nombre
			
  INTO	:ll_Numero, 
  			:ll_Productor, 
			:li_Especie, 
			:li_Variedad, 
			:li_Cantidad, 
  			:li_Vigencia, 
			:ls_frio, 
			:li_periodo, 
			:li_turno, 
			:li_linea, 
			:ls_Nombre
			
	FROM	dba.spro_ordenproceso op, 
			dba.variedades va
			
	WHERE	op.plde_codigo	=	:ai_Planta
	And	op.orpr_tipord		=	4
	And   op.orpr_numero	=	:al_Numero 
	And	va.espe_codigo		=	op.espe_codigo
	And	va.vari_codigo		=	op.vari_codigo
	AND   op.clie_codigo 		=  :uo_SelCliente.Codigo;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla Ordenes de Proceso")
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode <> 0 THEN
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

on w_gene_archi_plano.create
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
this.rb_2=create rb_2
this.gb_5=create gb_5
this.em_cajas=create em_cajas
this.st_7=create st_7
this.dw_3=create dw_3
this.st_2=create st_2
this.st_1=create st_1
this.st_6=create st_6
this.sle_mensa=create sle_mensa
this.pb_salir=create pb_salir
this.pb_grabar=create pb_grabar
this.gb_3=create gb_3
this.st_5=create st_5
this.rb_1=create rb_1
this.em_proceso=create em_proceso
this.cb_1=create cb_1
this.dw_2=create dw_2
this.Control[]={this.uo_selcliente,&
this.uo_selplanta,&
this.rb_2,&
this.gb_5,&
this.em_cajas,&
this.st_7,&
this.dw_3,&
this.st_2,&
this.st_1,&
this.st_6,&
this.sle_mensa,&
this.pb_salir,&
this.pb_grabar,&
this.gb_3,&
this.st_5,&
this.rb_1,&
this.em_proceso,&
this.cb_1,&
this.dw_2}
end on

on w_gene_archi_plano.destroy
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
destroy(this.rb_2)
destroy(this.gb_5)
destroy(this.em_cajas)
destroy(this.st_7)
destroy(this.dw_3)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.st_6)
destroy(this.sle_mensa)
destroy(this.pb_salir)
destroy(this.pb_grabar)
destroy(this.gb_3)
destroy(this.st_5)
destroy(this.rb_1)
destroy(this.em_proceso)
destroy(this.cb_1)
destroy(this.dw_2)
end on

event open;x = 0
y = 0

im_menu		= m_principal
This.Icon										=	Gstr_apl.Icono

This.Icon	=	Gstr_apl.Icono
pb_grabar.enabled = False

uo_SelPlanta.Seleccion(False, False)
uo_SelCliente.Seleccion(False, False)

uo_SelCliente.Inicia(gi_codexport)
uo_SelPlanta.Inicia(gstr_ParamPlanta.CodigoPlanta)

dw_2.SetTransObject(sqlca)
end event

event close;Boolean	Valida
Window	ventana
Integer	li_vta

ventana	= This.ParentWindow().GetFirstSheet()

IF IsValid(ventana) THEN
	li_vta++

	DO
		ventana	= this.ParentWindow().GetNextSheet(ventana)
		valida	= IsValid(ventana)
		IF valida THEN li_vta++
	LOOP WHILE valida
END IF
end event

type uo_selcliente from uo_seleccion_clientesprod within w_gene_archi_plano
integer x = 434
integer y = 124
integer height = 96
integer taborder = 100
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_gene_archi_plano
event destroy ( )
integer x = 2126
integer y = 124
integer height = 96
integer taborder = 60
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type rb_2 from radiobutton within w_gene_archi_plano
integer x = 539
integer y = 320
integer width = 530
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Despacho Emb."
end type

type gb_5 from groupbox within w_gene_archi_plano
integer x = 137
integer y = 236
integer width = 1586
integer height = 200
integer taborder = 100
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Documento de Salida"
end type

type em_cajas from editmask within w_gene_archi_plano
integer x = 2130
integer y = 264
integer width = 402
integer height = 84
integer taborder = 60
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 10789024
boolean enabled = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
string mask = "################"
end type

type st_7 from statictext within w_gene_archi_plano
integer x = 1861
integer y = 272
integer width = 265
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cajas"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_3 from datawindow within w_gene_archi_plano
boolean visible = false
integer x = 1024
integer y = 1808
integer width = 411
integer height = 432
integer taborder = 110
string title = "none"
string dataobject = "dw_mues_arch_planos_cajasprod"
boolean livescroll = true
end type

type st_2 from statictext within w_gene_archi_plano
integer x = 1861
integer y = 140
integer width = 229
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_1 from statictext within w_gene_archi_plano
integer x = 146
integer y = 140
integer width = 338
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_6 from statictext within w_gene_archi_plano
integer x = 59
integer y = 500
integer width = 3104
integer height = 164
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean enabled = false
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type sle_mensa from singlelineedit within w_gene_archi_plano
integer x = 87
integer y = 524
integer width = 3049
integer height = 116
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = script!
string facename = "Comic Sans MS"
boolean italic = true
long textcolor = 65535
long backcolor = 553648127
boolean displayonly = true
borderstyle borderstyle = stylelowered!
boolean hideselection = false
end type

type pb_salir from picturebutton within w_gene_archi_plano
integer x = 3250
integer y = 380
integer width = 302
integer height = 244
integer taborder = 100
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Apagar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Apagar-bn.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event clicked;Close(Parent)
end event

type pb_grabar from picturebutton within w_gene_archi_plano
integer x = 3250
integer y = 140
integer width = 302
integer height = 244
integer taborder = 90
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
boolean default = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar-bn.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event clicked;Parent.TriggerEvent("ue_guardar")
end event

type gb_3 from groupbox within w_gene_archi_plano
integer x = 105
integer y = 64
integer width = 2999
integer height = 400
integer taborder = 110
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
end type

type st_5 from statictext within w_gene_archi_plano
integer x = 59
integer y = 48
integer width = 3104
integer height = 448
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean enabled = false
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type rb_1 from radiobutton within w_gene_archi_plano
integer x = 174
integer y = 320
integer width = 306
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Proceso"
boolean checked = true
end type

type em_proceso from editmask within w_gene_archi_plano
integer x = 1125
integer y = 324
integer width = 402
integer height = 84
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;String		ls_nula
String ls_procdespa = " Despacho "

IF rb_1.Checked THEN ls_procdespa = " Proceso "
SetNull(ls_nula)

IF rb_1.Checked AND Not Existedocproceso(uo_SelPlanta.Codigo,Integer(This.Text)) THEN
	sle_mensa.Text	=	'No Existe el Proceso. Ingrese O Seleccione Otra'
	RETURN 0
ELSE
	sle_mensa.Text	=	'Buscando datos para el '+ls_procdespa+ String(Integer(em_proceso.Text), '00000000')
	IF NOT ValidaIngreso() THEN
		sle_mensa.Text	=	'El'+ls_procdespa+' no tiene cajas asociadas. Ingrese O Seleccione Otro'
		em_proceso.Text	=	ls_Nula
		RETURN 0
	END IF
END IF
end event

type cb_1 from commandbutton within w_gene_archi_plano
integer x = 1568
integer y = 320
integer width = 110
integer height = 84
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "..."
end type

event clicked;Parent.TriggerEvent("ue_seleccion")
end event

type dw_2 from datawindow within w_gene_archi_plano
boolean visible = false
integer x = 46
integer y = 788
integer width = 3351
integer height = 524
integer taborder = 120
boolean bringtotop = true
boolean titlebar = true
string dataobject = "dw_mues_arch_planos_cajasprod_despaemba"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean hsplitscroll = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;//String		ls_Tecla
//
//IF KeyDown(KeyShift!) THEN
//	ls_tecla	=	"Shift"
//ELSEIF KeyDown(KeyControl!) THEN
//	ls_tecla	=	"Control"
//END IF
//
//F_Selecciona(This, ls_Tecla, Row)
//
//IF dw_2.GetSelectedRow(0) = 0 THEN
//	pb_grabar.Enabled	=	False
//ELSE
//	pb_grabar.Enabled	=	True
//END IF
//
end event

