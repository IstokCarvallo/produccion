$PBExportHeader$w_gene_proceso_cuadratura.srw
$PBExportComments$Genera Datos para Pronóstico de Cierre
forward
global type w_gene_proceso_cuadratura from window
end type
type uo_selplanta from uo_seleccion_plantas within w_gene_proceso_cuadratura
end type
type uo_selcliente from uo_seleccion_clientesprod within w_gene_proceso_cuadratura
end type
type dw_2 from uo_dw within w_gene_proceso_cuadratura
end type
type st_4 from statictext within w_gene_proceso_cuadratura
end type
type rb_promedio from radiobutton within w_gene_proceso_cuadratura
end type
type rb_envase from radiobutton within w_gene_proceso_cuadratura
end type
type gb_peso from groupbox within w_gene_proceso_cuadratura
end type
type sle_espe from singlelineedit within w_gene_proceso_cuadratura
end type
type sle_codesp from singlelineedit within w_gene_proceso_cuadratura
end type
type cb_buscaorden from commandbutton within w_gene_proceso_cuadratura
end type
type em_fecha from editmask within w_gene_proceso_cuadratura
end type
type st_8 from statictext within w_gene_proceso_cuadratura
end type
type em_proceso from editmask within w_gene_proceso_cuadratura
end type
type st_7 from statictext within w_gene_proceso_cuadratura
end type
type ddlb_tipoproc from dropdownlistbox within w_gene_proceso_cuadratura
end type
type st_6 from statictext within w_gene_proceso_cuadratura
end type
type sle_mensa from singlelineedit within w_gene_proceso_cuadratura
end type
type st_5 from statictext within w_gene_proceso_cuadratura
end type
type pb_salir from picturebutton within w_gene_proceso_cuadratura
end type
type st_2 from statictext within w_gene_proceso_cuadratura
end type
type st_1 from statictext within w_gene_proceso_cuadratura
end type
type st_titulo from statictext within w_gene_proceso_cuadratura
end type
type r_1 from rectangle within w_gene_proceso_cuadratura
end type
type st_3 from statictext within w_gene_proceso_cuadratura
end type
type pb_acepta from picturebutton within w_gene_proceso_cuadratura
end type
type dw_3 from datawindow within w_gene_proceso_cuadratura
end type
type dw_1 from datawindow within w_gene_proceso_cuadratura
end type
end forward

global type w_gene_proceso_cuadratura from window
integer x = 1074
integer y = 484
integer width = 2546
integer height = 1468
boolean titlebar = true
string title = "Generación de Procesos de Cuadraturas"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 16777215
event ue_validapassword ( )
uo_selplanta uo_selplanta
uo_selcliente uo_selcliente
dw_2 dw_2
st_4 st_4
rb_promedio rb_promedio
rb_envase rb_envase
gb_peso gb_peso
sle_espe sle_espe
sle_codesp sle_codesp
cb_buscaorden cb_buscaorden
em_fecha em_fecha
st_8 st_8
em_proceso em_proceso
st_7 st_7
ddlb_tipoproc ddlb_tipoproc
st_6 st_6
sle_mensa sle_mensa
st_5 st_5
pb_salir pb_salir
st_2 st_2
st_1 st_1
st_titulo st_titulo
r_1 r_1
st_3 st_3
pb_acepta pb_acepta
dw_3 dw_3
dw_1 dw_1
end type
global w_gene_proceso_cuadratura w_gene_proceso_cuadratura

type variables
str_busqueda						istr_busq
Str_info								lstr_info

uo_especie							iuo_Especie
uo_control_historico_proceso	iuo_historico

Integer    							ii_TipoOrden, ii_tipord
end variables

forward prototypes
public subroutine habilitagrabar ()
public function boolean reprocesaorden (integer ai_tipoorden, long al_orden)
public function boolean noexisteordenreproceso (long al_orden)
public function boolean noexisteordenproceso (long al_orden)
public function boolean noexisteordendereembalaje (long al_orden)
public function boolean historial ()
end prototypes

event ue_validapassword();str_mant	lstr_mant

lstr_mant.Argumento[1]	=	"Granel - Genera Cuadratura Proceso"
lstr_mant.Argumento[2]	=	gstr_paramplanta.passpack

OpenWithParm(w_password, lstr_mant)

lstr_mant	=	Message.PowerObjectParm

IF lstr_mant.Respuesta = 0 THEN Close(This)
end event

public subroutine habilitagrabar ();
end subroutine

public function boolean reprocesaorden (integer ai_tipoorden, long al_orden);Integer	li_Estado, li_Cuenta
Boolean	lb_Retorno

SELECT	Count(*)
	INTO	:li_Cuenta
	FROM	dbo.spro_resultpacking
	WHERE	plde_codigo	=	:uo_SelPlanta.Codigo
	AND	orpr_tipdoc	=	:ai_TipoOrden
	AND	orpr_numero	=	:al_Orden ;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Tabla spro_resultpacking")
ELSEIF li_Cuenta > 0 THEN
	IF MessageBox("Atención", "Proceso Seleccionado ya Posee Cuadratura." + &
		"~r~rDesea reprocesar Cuadratura .", Question!, YesNo!, 2) = 1 THEN
	
		lb_Retorno	=	True
	END IF
ELSE
	lb_Retorno	=	True
END IF

RETURN lb_Retorno
end function

public function boolean noexisteordenreproceso (long al_orden);Integer	li_Especie, li_Estado
Date  		ldt_Fecha
Boolean	lb_Retorno = True


SELECT	espe_codigo, dinp_estado, dinp_fecdoc
	INTO	:li_Especie, :li_Estado, :ldt_Fecha
	FROM	dbo.spro_doctointernopack
	WHERE	plde_codigo	=	:uo_SelPlanta.Codigo
	AND	dinp_tipdoc	=	5
	AND	dinp_numero	=	:al_Orden
	AND 	clie_codigo =	:uo_SelCliente.Codigo;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca, "Lectura tabla spro_doctointernopack")
ELSEIF sqlca.SQLCode <> 100 THEN
	sle_codesp.text	=	String(li_Especie, '00')
	sle_espe.text		=	iuo_Especie.Nombre
	em_fecha.text		=	String(ldt_Fecha)
	
	IF li_Estado = 3 THEN
		MessageBox("Atención", "Orden de Proceso ya se encuentra Cerrada")
	ELSEIF li_Estado = 5 THEN
		MessageBox("Atención", "Orden de Proceso Cerrada Para Web.")
	ELSE
		lb_Retorno	=	False
	END IF	
END IF

RETURN lb_Retorno
end function

public function boolean noexisteordenproceso (long al_orden);Integer	li_Especie, li_Estado
Date  		ldt_Fecha
Boolean	lb_Retorno = True


SELECT	espe_codigo, orpr_estado, orpr_fecpro
	INTO	:li_Especie, :li_Estado, :ldt_Fecha
	FROM	dbo.spro_ordenproceso
	WHERE	plde_codigo	=	:uo_SelPlanta.Codigo
	AND 	clie_codigo =  :uo_SelCliente.Codigo
	AND	orpr_tipord	=	:ii_tipord
	AND	orpr_numero	=	:al_Orden;
		
If sqlca.SQLCode = -1 Then
	F_errorbasedatos(sqlca,"Lectura tabla Spro_Ordenproceso")
ElseIf sqlca.SQLCode <> 100 Then
	iuo_Especie.Existe(li_Especie, False, sqlca)

	sle_codesp.text		=	String(li_Especie, '00')
	sle_espe.text		=	iuo_Especie.Nombre
	em_fecha.text		=	String(ldt_Fecha)
	
	If li_Estado = 3 Then
		MessageBox("Atención", "Orden de Proceso ya se encuentra Cerrada")
	ElseIf li_Estado = 5 Then
		MessageBox("Atención", "Orden de Proceso Cerrada Para Web.")
	Else
		lb_Retorno	=	False
	End If	
End If

Return lb_Retorno
end function

public function boolean noexisteordendereembalaje (long al_orden);Integer	li_Especie, li_Estado
Date		ldt_Fecha
Boolean	lb_Retorno = True

SELECT	espe_codigo, dinp_estado, dinp_fecdoc
	INTO	:li_Especie, :li_Estado, :ldt_Fecha
	FROM	dbo.spro_doctointernopack
	WHERE	plde_codigo	=	:uo_SelPlanta.Codigo
	AND	dinp_tipdoc	=	6
	AND	dinp_numero	=	:al_Orden;
		
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla spro_doctointernopack")
ELSEIF sqlca.SQLCode <> 100 THEN
	sle_codesp.text	=	String(li_Especie, '00')
	sle_espe.text		=	iuo_Especie.Nombre
	em_fecha.text		=	String(ldt_Fecha)
	
	IF li_Estado = 3 THEN
		MessageBox("Atención", "Orden de Re - Embalaje ya se encuentra Cerrada")
	ELSEIF li_Estado = 5 THEN
		MessageBox("Atención", "Orden de Proceso Cerrada Para Web.")
	ELSE
		lb_Retorno	=	False
	END IF	
END IF

RETURN lb_Retorno
end function

public function boolean historial ();Boolean			lb_retorno	=	True
Integer			ll_fila
DwItemStatus	li_dwitemstatus
Integer			li_tipord, li_codmod, li_tipmov
Long				ll_proceso
Date 				ld_fecmov
Time				lt_hormov
String 			ls_pcname

ll_fila = 1
	
li_dwitemstatus	=	dw_2.GetItemStatus(ll_Fila, 0, Primary!)
li_tipord			=	ii_TipoOrden
ll_proceso			=	Long(em_proceso.text)
ld_fecmov			=	Date(f_fechahora())
lt_hormov			=	Time(f_fechahora())
ls_pcname			=	gstr_us.computador
		
li_tipmov			=	6//Generación de Resultado de Proceso
li_codmod			=	1

lb_retorno			=	iuo_historico.InsertaHistoria(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, li_tipord, &
																	ll_proceso, li_codmod, li_tipmov, ld_fecmov,  lt_hormov, ls_pcname, &
																	True, Sqlca)
Return lb_retorno
end function

on w_gene_proceso_cuadratura.create
this.uo_selplanta=create uo_selplanta
this.uo_selcliente=create uo_selcliente
this.dw_2=create dw_2
this.st_4=create st_4
this.rb_promedio=create rb_promedio
this.rb_envase=create rb_envase
this.gb_peso=create gb_peso
this.sle_espe=create sle_espe
this.sle_codesp=create sle_codesp
this.cb_buscaorden=create cb_buscaorden
this.em_fecha=create em_fecha
this.st_8=create st_8
this.em_proceso=create em_proceso
this.st_7=create st_7
this.ddlb_tipoproc=create ddlb_tipoproc
this.st_6=create st_6
this.sle_mensa=create sle_mensa
this.st_5=create st_5
this.pb_salir=create pb_salir
this.st_2=create st_2
this.st_1=create st_1
this.st_titulo=create st_titulo
this.r_1=create r_1
this.st_3=create st_3
this.pb_acepta=create pb_acepta
this.dw_3=create dw_3
this.dw_1=create dw_1
this.Control[]={this.uo_selplanta,&
this.uo_selcliente,&
this.dw_2,&
this.st_4,&
this.rb_promedio,&
this.rb_envase,&
this.gb_peso,&
this.sle_espe,&
this.sle_codesp,&
this.cb_buscaorden,&
this.em_fecha,&
this.st_8,&
this.em_proceso,&
this.st_7,&
this.ddlb_tipoproc,&
this.st_6,&
this.sle_mensa,&
this.st_5,&
this.pb_salir,&
this.st_2,&
this.st_1,&
this.st_titulo,&
this.r_1,&
this.st_3,&
this.pb_acepta,&
this.dw_3,&
this.dw_1}
end on

on w_gene_proceso_cuadratura.destroy
destroy(this.uo_selplanta)
destroy(this.uo_selcliente)
destroy(this.dw_2)
destroy(this.st_4)
destroy(this.rb_promedio)
destroy(this.rb_envase)
destroy(this.gb_peso)
destroy(this.sle_espe)
destroy(this.sle_codesp)
destroy(this.cb_buscaorden)
destroy(this.em_fecha)
destroy(this.st_8)
destroy(this.em_proceso)
destroy(this.st_7)
destroy(this.ddlb_tipoproc)
destroy(this.st_6)
destroy(this.sle_mensa)
destroy(this.st_5)
destroy(this.pb_salir)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.st_titulo)
destroy(this.r_1)
destroy(this.st_3)
destroy(this.pb_acepta)
destroy(this.dw_3)
destroy(this.dw_1)
end on

event open;Boolean lb_Cerrar

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	X	=	0
	Y	=	0
	
	uo_SelPlanta.Seleccion(False, False)
	uo_SelCliente.Seleccion(False, False)
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlanta.Inicia(gstr_paramplanta.CodigoPlanta)
	
	dw_1.SetTransObject(SQLCA)
	dw_2.SetTransObject(SQLCA)
	dw_3.SetTransObject(SQLCA)
	
	em_fecha.text			=	String(Date(Today()),'dd/mm/yyyy')
	ii_TipoOrden			=	1
	ddlb_tipoproc.Text 	= 	'1. - Proceso'
	
	iuo_Especie				=	Create uo_Especie
	iuo_historico			=	Create uo_control_historico_proceso
End If
end event

type uo_selplanta from uo_seleccion_plantas within w_gene_proceso_cuadratura
event destroy ( )
integer x = 590
integer y = 352
integer height = 92
integer taborder = 60
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selcliente from uo_seleccion_clientesprod within w_gene_proceso_cuadratura
event destroy ( )
integer x = 590
integer y = 240
integer height = 92
integer taborder = 40
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type dw_2 from uo_dw within w_gene_proceso_cuadratura
boolean visible = false
integer x = 649
integer y = 1380
integer width = 1042
integer height = 728
integer taborder = 90
boolean titlebar = true
string title = "Pallets Con Anomalías"
string dataobject = "dw_proceso_cuadratura_pallets"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

type st_4 from statictext within w_gene_proceso_cuadratura
integer x = 165
integer y = 244
integer width = 370
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Cliente"
boolean focusrectangle = false
end type

type rb_promedio from radiobutton within w_gene_proceso_cuadratura
integer x = 1038
integer y = 912
integer width = 887
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Kilos Promedio del Proceso"
end type

type rb_envase from radiobutton within w_gene_proceso_cuadratura
integer x = 251
integer y = 912
integer width = 690
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Peso de Cuadratura"
boolean checked = true
end type

type gb_peso from groupbox within w_gene_proceso_cuadratura
integer x = 165
integer y = 836
integer width = 1838
integer height = 200
integer taborder = 70
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Peso de Referencia"
end type

type sle_espe from singlelineedit within w_gene_proceso_cuadratura
integer x = 777
integer y = 708
integer width = 690
integer height = 92
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type sle_codesp from singlelineedit within w_gene_proceso_cuadratura
integer x = 590
integer y = 708
integer width = 174
integer height = 92
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type cb_buscaorden from commandbutton within w_gene_proceso_cuadratura
integer x = 1015
integer y = 592
integer width = 96
integer height = 80
integer taborder = 30
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "..."
end type

event clicked;Str_busqueda	lstr_busq

CHOOSE CASE ii_TipoOrden
	CASE 1
		lstr_busq.argum[1]	=	String(uo_SelPlanta.Codigo)
		lstr_busq.argum[2]	=	"2"
		lstr_busq.argum[3]	=	"4"
		lstr_busq.argum[6]	=	""
		
		OpenWithParm(w_busc_spro_ordenproceso, lstr_busq)
		
		lstr_busq	= Message.PowerObjectParm
		
		IF lstr_busq.argum[6] <> "" THEN
			IF ReprocesaOrden(ii_TipoOrden + 3, Long(lstr_busq.argum[6])) THEN
				em_proceso.text	=	lstr_busq.argum[6]
				
				IF NoExisteOrdenProceso(Long(lstr_busq.argum[6])) THEN
					 em_proceso.text=""
					 
				END IF	
			END IF
		END IF
		
		RETURN 1
		
	CASE 2
		
		lstr_Busq.Argum[1]	=	'5'	// Tipo Orden

		OpenWithParm(w_busqueda_doctointernopack, lstr_Busq)

		lstr_Busq	= Message.PowerObjectParm

		IF lstr_Busq.Argum[2] <> "" THEN
			em_proceso.text	=	lstr_busq.argum[3]
			IF NoExisteOrdenReProceso(Long(lstr_Busq.Argum[3])) THEN
				em_proceso.Text=""
			END IF
		END IF
	
	CASE 3
		
		lstr_Busq.Argum[1]	=	'6'	// Tipo Orden

		OpenWithParm(w_busqueda_doctointernopack, lstr_Busq)

		lstr_Busq	= Message.PowerObjectParm

		IF lstr_Busq.Argum[2] <> "" THEN
			em_proceso.text	=	lstr_busq.argum[3]
			IF NoExisteOrdendeReEmbalaje(Long(lstr_Busq.Argum[3])) THEN
				em_proceso.Text=""
			END IF
		END IF
		
	CASE 8
		lstr_busq.argum[1]	=	String(uo_SelPlanta.Codigo)
		lstr_busq.argum[2]	=	"2"
		lstr_busq.argum[3]	=	"8"
		lstr_busq.argum[6]	=	""
		
		OpenWithParm(w_busc_spro_ordenproceso, lstr_busq)
		
		lstr_busq	= Message.PowerObjectParm
		
		IF lstr_busq.argum[6] <> "" THEN
			IF ReprocesaOrden(ii_TipoOrden, Long(lstr_busq.argum[6])) THEN
				em_proceso.text	=	lstr_busq.argum[6]
				
				IF NoExisteOrdenProceso(Long(lstr_busq.argum[6])) THEN
					 em_proceso.text=""
					 
				END IF	
			END IF
		END IF
		
		RETURN 1
		
END CHOOSE
end event

type em_fecha from editmask within w_gene_proceso_cuadratura
integer x = 1568
integer y = 584
integer width = 439
integer height = 92
integer taborder = 40
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 1090519039
string text = "none"
alignment alignment = center!
boolean displayonly = true
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_8 from statictext within w_gene_proceso_cuadratura
integer x = 1353
integer y = 596
integer width = 229
integer height = 76
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_proceso from editmask within w_gene_proceso_cuadratura
integer x = 590
integer y = 588
integer width = 384
integer height = 92
integer taborder = 20
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 1090519039
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;Long ll_orden

ll_orden = long(this.text)

IF (ii_tipoorden <> 8 AND ReprocesaOrden(ii_TipoOrden + 3, ll_Orden)) OR (ii_tipoorden = 8 AND ReprocesaOrden(ii_TipoOrden, ll_Orden)) THEN
	CHOOSE CASE ii_TipoOrden
		CASE 1
			ii_tipord	=	4
			IF NoExisteOrdenProceso(ll_orden) THEN
				this.Text=""
			END IF
			
		CASE 2
			IF NoExisteOrdenReProceso(ll_orden) THEN
				this.Text=""
			END IF
	
		CASE 3
			IF NoExisteOrdendeReEmbalaje(ll_orden) THEN
				this.Text=""
			END IF
			
		CASE 8
			ii_tipord	=	8
			IF NoExisteOrdenProceso(ll_orden) THEN
				this.Text=""
			END IF
	END CHOOSE
ELSE
	this.Text=""
END IF

IF sle_codesp.text <> "" THEN
	IF Not manbin_especie(uo_SelPlanta.Codigo, Integer(sle_codesp.text), True, sqlca) THEN
		this.Text 			=	""
		sle_codesp.text 	=	""
		em_fecha.text		=	""
	END IF
END IF
end event

type st_7 from statictext within w_gene_proceso_cuadratura
integer x = 165
integer y = 600
integer width = 370
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Nº Proceso"
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type ddlb_tipoproc from dropdownlistbox within w_gene_proceso_cuadratura
integer x = 590
integer y = 468
integer width = 869
integer height = 360
integer taborder = 10
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 16777215
string text = "none"
string item[] = {"1. - Proceso","2. - Re Proceso","3. - Re Embalaje","4. - Pre Proceso"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;CHOOSE CASE Index
		
	CASE 2
		ii_TipoOrden	=	5
		
	CASE 3
		ii_TipoOrden	=	7
		
	CASE 4
		ii_TipoOrden	=	8
		
	CASE ELSE
		ii_TipoOrden	=	Index
		
END CHOOSE

sle_mensa.text			= 	""
end event

type st_6 from statictext within w_gene_proceso_cuadratura
integer x = 165
integer y = 356
integer width = 370
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type sle_mensa from singlelineedit within w_gene_proceso_cuadratura
integer x = 137
integer y = 1164
integer width = 1888
integer height = 116
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 65535
long backcolor = 553648127
boolean autohscroll = false
borderstyle borderstyle = stylelowered!
end type

type st_5 from statictext within w_gene_proceso_cuadratura
integer x = 82
integer y = 1100
integer width = 1993
integer height = 224
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean enabled = false
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type pb_salir from picturebutton within w_gene_proceso_cuadratura
integer x = 2162
integer y = 764
integer width = 302
integer height = 244
integer taborder = 60
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Apagar.png"
alignment htextalign = right!
long backcolor = 553648127
end type

event clicked;Close(Parent)
end event

type st_2 from statictext within w_gene_proceso_cuadratura
integer x = 165
integer y = 468
integer width = 393
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Tipo Proceso"
boolean focusrectangle = false
end type

type st_1 from statictext within w_gene_proceso_cuadratura
integer x = 165
integer y = 712
integer width = 370
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Especie"
boolean focusrectangle = false
end type

type st_titulo from statictext within w_gene_proceso_cuadratura
integer x = 82
integer y = 68
integer width = 1993
integer height = 92
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
string text = "Proceso de Cuadratura"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type r_1 from rectangle within w_gene_proceso_cuadratura
long linecolor = 12632256
integer linethickness = 4
long fillcolor = 33543637
integer x = 773
integer y = 648
integer width = 165
integer height = 144
end type

type st_3 from statictext within w_gene_proceso_cuadratura
integer x = 82
integer y = 192
integer width = 1993
integer height = 908
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean enabled = false
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type pb_acepta from picturebutton within w_gene_proceso_cuadratura
event clicked pbm_bnclicked
integer x = 2162
integer y = 440
integer width = 302
integer height = 256
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean default = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Guardar Como.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Guardar Como-bn.png"
alignment htextalign = right!
long backcolor = 553648127
end type

event clicked;Integer		li_Especie, li_TipoOrden, li_TipoPeso, li_control
Long			ll_Numero
String		ls_Usuario, ls_Password, ls_nombre

String		las_prepro[]	=	{'Proceso terminado satisfactoriamente',				&
										 'Error Elimina spro_resulfrutemba',					&
										 'Error Elimina spro_resulfrutrecepcion',				&
										 'Error Elimina Tabla spro_resulfrutcomer',			&
										 'Error Elimina Tabla spro_resulpackingdeta',		&
										 'Error Elimina Tablaspro_resultpacking',				&
										 'Error Grabar spro_resultpacking',		&
										 'Error Grabar spro_resulfrutrecepcion',	&
										 'Error Grabar spro_resulpackingdeta',					&
										 'Error Grabar spro_resulfrutemba',						&
										 'Error Grabar spro_resulfrutcomer'}
String		las_pro[]		=	{'Proceso terminado satisfactoriamente',				&
										 'Error Elimina spro_resulfrutemba',					&
										 'Error Elimina spro_resulfrutrecepcion',				&
										 'Error Elimina Tabla spro_resulfrutcomer',			&
										 'Error Elimina Tabla spro_resulpackingdeta',		&
										 'Error Elimina Tablaspro_resultpacking',				&
										 'Error Grabar spro_resultpacking',		&
										 'Error Grabar spro_resulfrutrecepcion',	&
										 'Error Grabar spro_resulpackingdeta',					&
										 'Error Grabar spro_resulfrutemba',						&
										 'Error Grabar spro_resulfrutcomer'}

Transaction	sqlproc

SetPointer(HourGlass!)

Parent.Height	=	1550

sle_mensa.text	=	""
li_Especie		=	Integer(sle_codesp.text)
ll_Numero		=	Long(em_proceso.text)


If IsNull(ii_TipoOrden) Then
	MessageBox("Atención","Debe Seleccionar Tipo de Proceso",Exclamation!)
	Return
ElseIf ii_TipoOrden = 8 Then
	li_TipoOrden 	= 	ii_TipoOrden
ElseIf ii_TipoOrden = 5 Then
	li_TipoOrden 	= 	ii_TipoOrden
ElseIf ii_TipoOrden = 7 Then
	li_TipoOrden 	= 	ii_TipoOrden
Else
	li_TipoOrden	=	ii_TipoOrden + 3
End If

If IsNull(ll_Numero) OR ll_Numero =0 Then
	MessageBox("Atención","Debe Seleccionar Número de Proceso.",Exclamation!)
	Return
End If	

If rb_Envase.Checked Then
	li_TipoPeso		=	1
Else
	li_TipoPeso		=	2
End If

If dw_2.Retrieve(uo_SelPlanta.Codigo, li_TipoOrden, ll_Numero, uo_SelCliente.Codigo)= 0 Then
	If dw_1.Retrieve(uo_SelPlanta.Codigo, li_TipoOrden, ll_Numero, li_TipoPeso, uo_SelCliente.Codigo) = 0 Then
		If dw_3.Retrieve(uo_SelPlanta.Codigo, li_TipoOrden, ll_Numero, uo_SelCliente.Codigo) = 0 Then
		
//			sqlproc					= 	Create Transaction
//		
//			ls_Usuario				=	sqlca.UserId
//			ls_Password				=	sqlca.DbPass
//			ls_nombre				=	ProfileString(gstr_apl.ini, gs_Base, "NombreOdbc", "")
//			SqlProc.Dbms			=	sqlca.Dbms
//			SqlProc.ServerName	=	sqlca.ServerName
//			SqlProc.DataBase		=	sqlca.DataBase
//			
//			SqlProc.DbParm			= 	"Connectstring='DSN=" + ls_nombre + ";UID=" + ls_Usuario + &
//										  	";PWD=" + ls_Password + "'// ;PBUseProcOwner = " + '"Yes"'
//			CONNECT USING SqlProc;
			
			If SQLCA.SQLCode = -1 Then
				F_ErrorBaseDatos(SQLCA, "Lectura del Procedimiento Almacenado Genera_ResulProcesoPacking" )		
				sle_mensa.text	=	"Error al Generar Datos."
			Else
				If li_TipoOrden <> 8 Then
					DECLARE	OrdenProceso PROCEDURE FOR dbo.Genera_ResulProcesoPacking
						@Planta 				=	:uo_SelPlanta.Codigo,   
						@TipoOrden		=	:li_TipoOrden,
						@NumeroOrden	=	:ll_Numero, 
						@TipoPeso			=	:li_TipoPeso,
						@Cliente				=	:uo_SelCliente.Codigo
					USING SQLCA ;
							
					EXECUTE OrdenProceso;	
					
					If SQLCA.SQLCode = -1 Then
						F_ErrorBaseDatos(sqlca, "Lectura del Procedimiento Almacenado Genera_ResulProcesoPacking" )				
						sle_mensa.text	=	"Error al Generar Datos."
						Rollback;
					Else
						FETCH OrdenProceso into :li_control;
						
						If li_control = 0 Then
							Commit;
							Historial()
						Else 
							rollback;
						End If
						sle_mensa.text	=	las_pro[li_control + 1]
					End If	
						
					CLOSE   OrdenProceso;
				Else
					DECLARE	OrdenPreProceso PROCEDURE FOR dbo.Genera_ResulPreProcesoPacking
								@Planta 				=	:uo_SelPlanta.Codigo,   
								@TipoOrden		=	:li_TipoOrden,
								@NumeroOrden	=	:ll_Numero, 
								@TipoPeso			=	:li_TipoPeso,
								@Cliente				=	:uo_SelCliente.Codigo
					USING SQLCA ;
							
					EXECUTE OrdenPreProceso;	
									
					If SQLCA.SQLCode = -1 Then
						F_ErrorBaseDatos(sqlca, "Lectura del Procedimiento Almacenado " + &
														"Genera_ResulPreProcesoPacking" )
												
						sle_mensa.text	=	"Error al Generar Datos."
						Rollback;
					Else
						FETCH OrdenPreProceso into :li_control;
						
						If li_control = 0 Then
							Commit;
							Historial()
						Else 
							Rollback;
						End If
						sle_mensa.text	=	las_prepro[li_control + 1]
					End If	
						
					CLOSE   OrdenPreProceso;
				End If
			End If
			
//			DISCONNECT USING SqlProc;
//			
//			Destroy SqlProc
		Else
			MessageBox("Atención","Existen Calibres no registrados, que impiden realizar el Proceso")
			dw_1.Visible	=	False
			dw_2.Visible	=	False
			dw_3.Visible	=	True
			Parent.Height	=	2180
		End If
	Else
		MessageBox("Atención","Existen Pesos no registrados, que impiden realizar el Proceso")
		dw_1.Visible	=	True
		dw_2.Visible	=	False
		dw_3.Visible	=	False
		Parent.Height	=	2180
	End If
Else
	MessageBox("Atención","Existen Pallets con anomalías, que impiden realizar el Proceso")
	dw_2.Visible	=	True
	dw_1.Visible	=	False
	dw_3.Visible	=	False
	Parent.Height	=	2180
End If
	
SetPointer(Arrow!)
end event

type dw_3 from datawindow within w_gene_proceso_cuadratura
boolean visible = false
integer x = 37
integer y = 1376
integer width = 1819
integer height = 728
integer taborder = 90
boolean titlebar = true
string title = "Detalle de Resultado sin Calibre"
string dataobject = "dw_detalle_resultado_sin_calibre"
boolean hscrollbar = true
boolean vscrollbar = true
boolean hsplitscroll = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_1 from datawindow within w_gene_proceso_cuadratura
boolean visible = false
integer x = 37
integer y = 1376
integer width = 2354
integer height = 728
integer taborder = 80
boolean titlebar = true
string title = "Embalajes sin Pesaje"
string dataobject = "dw_cons_embalajes_sin_pesaje"
boolean hscrollbar = true
boolean vscrollbar = true
boolean hsplitscroll = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

