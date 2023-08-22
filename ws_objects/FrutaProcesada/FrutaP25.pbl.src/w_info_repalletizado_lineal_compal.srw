$PBExportHeader$w_info_repalletizado_lineal_compal.srw
$PBExportComments$Informe de Repalletizajes.
forward
global type w_info_repalletizado_lineal_compal from w_para_informes
end type
type gb_3 from groupbox within w_info_repalletizado_lineal_compal
end type
type st_4 from statictext within w_info_repalletizado_lineal_compal
end type
type st_1 from statictext within w_info_repalletizado_lineal_compal
end type
type st_2 from statictext within w_info_repalletizado_lineal_compal
end type
type em_numero from editmask within w_info_repalletizado_lineal_compal
end type
type st_6 from statictext within w_info_repalletizado_lineal_compal
end type
type cb_buscarepa from commandbutton within w_info_repalletizado_lineal_compal
end type
type cbx_etiq from checkbox within w_info_repalletizado_lineal_compal
end type
type st_5 from statictext within w_info_repalletizado_lineal_compal
end type
type st_8 from statictext within w_info_repalletizado_lineal_compal
end type
type st_3 from statictext within w_info_repalletizado_lineal_compal
end type
type st_variedad from statictext within w_info_repalletizado_lineal_compal
end type
type st_12 from statictext within w_info_repalletizado_lineal_compal
end type
type em_calidad from editmask within w_info_repalletizado_lineal_compal
end type
type cbx_calidad from checkbox within w_info_repalletizado_lineal_compal
end type
type st_embalaje from statictext within w_info_repalletizado_lineal_compal
end type
type cbx_embalaje from checkbox within w_info_repalletizado_lineal_compal
end type
type em_embalaje from editmask within w_info_repalletizado_lineal_compal
end type
type cb_buscaembalaje from commandbutton within w_info_repalletizado_lineal_compal
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_repalletizado_lineal_compal
end type
type uo_selplantas from uo_seleccion_plantas within w_info_repalletizado_lineal_compal
end type
type uo_selespecie from uo_seleccion_especie within w_info_repalletizado_lineal_compal
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_repalletizado_lineal_compal
end type
type uo_selproductor from uo_seleccion_productor within w_info_repalletizado_lineal_compal
end type
end forward

global type w_info_repalletizado_lineal_compal from w_para_informes
integer x = 14
integer y = 32
integer width = 2697
integer height = 2112
string title = "Producción en Frigorificos"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
gb_3 gb_3
st_4 st_4
st_1 st_1
st_2 st_2
em_numero em_numero
st_6 st_6
cb_buscarepa cb_buscarepa
cbx_etiq cbx_etiq
st_5 st_5
st_8 st_8
st_3 st_3
st_variedad st_variedad
st_12 st_12
em_calidad em_calidad
cbx_calidad cbx_calidad
st_embalaje st_embalaje
cbx_embalaje cbx_embalaje
em_embalaje em_embalaje
cb_buscaembalaje cb_buscaembalaje
uo_selcliente uo_selcliente
uo_selplantas uo_selplantas
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
uo_selproductor uo_selproductor
end type
global w_info_repalletizado_lineal_compal w_info_repalletizado_lineal_compal

type variables
str_busqueda istr_busq
str_mant istr_mant

Integer ii_etiq, ii_TipoRepa

uo_calibre	iuo_calibre
end variables

forward prototypes
public function boolean noexistefolio (long al_numero)
public function boolean existeproductor (long ll_productor)
end prototypes

public function boolean noexistefolio (long al_numero);Integer	li_TipoRepa
Long		ll_Numero,ll_CantTarjas,ll_CantParticipa,ll_CantInspec
Date		ld_FechaRepa

ll_Numero	=	Long(em_numero.Text)

SELECT	repe_fecrep, repe_tipopa
	INTO	:ld_FechaRepa, :ii_TipoRepa
	FROM	dbo.REPALLETENCA
	WHERE	clie_codigo	=	:uo_SelCliente.Codigo
	AND	plde_codigo	=	:uo_SelPlantas.Codigo
	AND	repe_numero	=	:al_Numero ;
			
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Repalletizados")
	
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Número de Repalletizado no ha sido Ingresado.~r~r" + &
					"Ingrese o seleccione otro Número.", Exclamation!, Ok!)
					
	RETURN True
ELSE
	SELECT	Count(paen_numero)
		INTO	:ll_CantTarjas
		FROM	dbo.REPALLETDETA
		WHERE	clie_codigo	=	:uo_SelCliente.Codigo
		AND	plde_codigo	=	:uo_SelPlantas.Codigo
		AND	repe_numero	=	:ll_Numero
		AND	repd_tipood	=	1;
				
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Detalle de Repalletizajes")
			
			RETURN True
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Error", "Número de Repalletizaje no tiene Detalle.~r~r" + &
						"Consulte con Encargado.", Exclamation!, Ok!)
			
			RETURN True
	ELSE
		SELECT	Count(paen_numero)
			INTO	:ll_CantParticipa
			FROM	dbo.REPALLETDETA
			WHERE	clie_codigo	=	:uo_SelCliente.Codigo
			AND	plde_codigo	=	:uo_SelPlantas.Codigo
			AND	repe_numero	=	:ll_Numero;
		
		SELECT	Count(rep.paen_numero)
			INTO	:ll_CantInspec
			FROM	dbo.REPALLETDETA as rep, dbo.PALLETENCAB as pae
			WHERE	rep.clie_codigo	=	:uo_SelCliente.Codigo
			AND	rep.plde_codigo	=	:uo_SelPlantas.Codigo
			AND	rep.repe_numero	=	:ll_Numero
			AND	rep.repd_tipood	=	1
			AND	pae.clie_codigo	=	rep.clie_codigo
			AND	pae.plde_codigo	=	rep.plde_codigo
			AND	pae.paen_numero	=	rep.paen_numero			
			AND	IsNull(pae.paen_inspec, 0)	> 0 ;
					
		IF sqlca.SQLCode = -1 THEN
			F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Detalle de Repalletizajes")
			
			RETURN True

		END IF
	END IF
	
	
	RETURN False
END IF
end function

public function boolean existeproductor (long ll_productor);String	ls_Nombre

SELECT	prod_nombre
	INTO	:ls_Nombre
	FROM	dbo.productores
	WHERE	prod_codigo	=	:ll_Productor ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Productor")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Productor no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	istr_mant.argumento[6] = String(ll_Productor)	
	RETURN True
END IF
end function

on w_info_repalletizado_lineal_compal.create
int iCurrent
call super::create
this.gb_3=create gb_3
this.st_4=create st_4
this.st_1=create st_1
this.st_2=create st_2
this.em_numero=create em_numero
this.st_6=create st_6
this.cb_buscarepa=create cb_buscarepa
this.cbx_etiq=create cbx_etiq
this.st_5=create st_5
this.st_8=create st_8
this.st_3=create st_3
this.st_variedad=create st_variedad
this.st_12=create st_12
this.em_calidad=create em_calidad
this.cbx_calidad=create cbx_calidad
this.st_embalaje=create st_embalaje
this.cbx_embalaje=create cbx_embalaje
this.em_embalaje=create em_embalaje
this.cb_buscaembalaje=create cb_buscaembalaje
this.uo_selcliente=create uo_selcliente
this.uo_selplantas=create uo_selplantas
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
this.uo_selproductor=create uo_selproductor
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_3
this.Control[iCurrent+2]=this.st_4
this.Control[iCurrent+3]=this.st_1
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.em_numero
this.Control[iCurrent+6]=this.st_6
this.Control[iCurrent+7]=this.cb_buscarepa
this.Control[iCurrent+8]=this.cbx_etiq
this.Control[iCurrent+9]=this.st_5
this.Control[iCurrent+10]=this.st_8
this.Control[iCurrent+11]=this.st_3
this.Control[iCurrent+12]=this.st_variedad
this.Control[iCurrent+13]=this.st_12
this.Control[iCurrent+14]=this.em_calidad
this.Control[iCurrent+15]=this.cbx_calidad
this.Control[iCurrent+16]=this.st_embalaje
this.Control[iCurrent+17]=this.cbx_embalaje
this.Control[iCurrent+18]=this.em_embalaje
this.Control[iCurrent+19]=this.cb_buscaembalaje
this.Control[iCurrent+20]=this.uo_selcliente
this.Control[iCurrent+21]=this.uo_selplantas
this.Control[iCurrent+22]=this.uo_selespecie
this.Control[iCurrent+23]=this.uo_selvariedad
this.Control[iCurrent+24]=this.uo_selproductor
end on

on w_info_repalletizado_lineal_compal.destroy
call super::destroy
destroy(this.gb_3)
destroy(this.st_4)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.em_numero)
destroy(this.st_6)
destroy(this.cb_buscarepa)
destroy(this.cbx_etiq)
destroy(this.st_5)
destroy(this.st_8)
destroy(this.st_3)
destroy(this.st_variedad)
destroy(this.st_12)
destroy(this.em_calidad)
destroy(this.cbx_calidad)
destroy(this.st_embalaje)
destroy(this.cbx_embalaje)
destroy(this.em_embalaje)
destroy(this.cb_buscaembalaje)
destroy(this.uo_selcliente)
destroy(this.uo_selplantas)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
destroy(this.uo_selproductor)
end on

event open;call super::open;Boolean	lb_Cerrar

If IsNUll(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNUll(uo_SelPlantas.Codigo) Then lb_Cerrar = True
If IsNUll(uo_SelEspecie.Codigo) Then lb_Cerrar = True
If IsNUll(uo_SelVariedad.Codigo) Then lb_Cerrar = True
If IsNUll(uo_SelProductor.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlantas.Seleccion(False, False)
	uo_SelEspecie.Seleccion(False, False)
	uo_SelEspecie.Seleccion(True, False)
	uo_SelProductor.Seleccion(True, False)
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlantas.Inicia(gi_CodPlanta)
	//uo_SelEspecie.Inicia(gi_CodEspecie)
	uo_SelVariedad.Filtra(gi_CodEspecie)
	
	iuo_calibre   						=	Create uo_calibre
	
	istr_mant.argumento[2]	=	""
	istr_mant.argumento[11]	=	'*'							// Calibre
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_repalletizado_lineal_compal
end type

type st_computador from w_para_informes`st_computador within w_info_repalletizado_lineal_compal
end type

type st_usuario from w_para_informes`st_usuario within w_info_repalletizado_lineal_compal
end type

type st_temporada from w_para_informes`st_temporada within w_info_repalletizado_lineal_compal
end type

type p_logo from w_para_informes`p_logo within w_info_repalletizado_lineal_compal
end type

type st_titulo from w_para_informes`st_titulo within w_info_repalletizado_lineal_compal
integer width = 1902
string text = "Informe de Cuadratura Repalletizado"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_repalletizado_lineal_compal
string tag = "Imprimir Reporte"
integer x = 2304
integer y = 1248
integer taborder = 50
end type

event pb_acepta::clicked;SetPointer(Arrow!)
Integer	fila,li_numero
String		ls_embalaje = 'Z'

IF Not cbx_embalaje.Checked THEN ls_embalaje = em_embalaje.Text

istr_info.titulo	= 'INFORME DE CUADRATURA REPALLETIZADO'
OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_repa_cuadra_lineal"
vinf.dw_1.SetTransObject(sqlca)

IF em_numero.text ="" THEN
	istr_mant.argumento[2] = '-1'
END IF

fila	=	vinf.dw_1.Retrieve(uo_SelPlantas.Codigo, Long(istr_mant.Argumento[2]), &
										uo_SelCliente.Codigo,ii_etiq,uo_SelProductor.Codigo,istr_mant.Argumento[11],&
										ls_embalaje,uo_SelVariedad.Codigo, uo_SelEspecie.Codigo)
	
	IF fila = -1 THEN
		MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
						"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
	
	ELSEIF fila = 0 THEN
		MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
	ELSE
		F_Membrete(vinf.dw_1)
		istr_mant.Argumento[6] = 'Completar Pallet'
		istr_mant.Argumento[7] = 'Reembalaje'
		istr_mant.Argumento[15] = 'Cambio de Altura'
		istr_mant.Argumento[16] = 'Cambio de Altura'
		
		IF ii_TipoRepa = 3 THEN
			vinf.dw_1.Modify("tipopa.text = '" + istr_mant.Argumento[6] + "'")
		ELSEIF ii_TipoRepa = 7 THEN
			vinf.dw_1.Modify("tipopa.text = '" + istr_mant.Argumento[7] + "'")
		ELSEIF ii_TipoRepa =1 THEN
			vinf.dw_1.Modify("tipopa.text = '" + istr_mant.Argumento[15] + "'")
		ELSEIF ii_TipoRepa = 2 THEN
			vinf.dw_1.Modify("tipopa.text = '" + istr_mant.Argumento[16] + "'")	
		END IF	
		
		vinf.dw_1.Modify("planta.text = '" + uo_SelPlantas.Nombre + "'")
		IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_repalletizado_lineal_compal
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2295
integer y = 1564
integer taborder = 70
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type gb_3 from groupbox within w_info_repalletizado_lineal_compal
integer x = 1495
integer y = 1524
integer width = 603
integer height = 256
integer taborder = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
borderstyle borderstyle = styleraised!
end type

type st_4 from statictext within w_info_repalletizado_lineal_compal
integer x = 256
integer y = 440
integer width = 1902
integer height = 1076
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_repalletizado_lineal_compal
integer x = 306
integer y = 596
integer width = 462
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
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_repalletizado_lineal_compal
integer x = 361
integer y = 1636
integer width = 535
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Nro. Repaletizado"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_numero from editmask within w_info_repalletizado_lineal_compal
integer x = 910
integer y = 1624
integer width = 393
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;IF This.Text <> "" THEN
	IF NoExisteFolio(Long(This.Text)) THEN
		This.Text	=	""
		
		This.SetFocus()
	ELSE
		istr_mant.argumento[2]	=	String(Long(This.Text), '00000000')
	END IF
END IF

IF this.text <> '' and NOT isnull(This.text) THEN cbx_etiq.enabled = true
end event

type st_6 from statictext within w_info_repalletizado_lineal_compal
integer x = 306
integer y = 472
integer width = 233
integer height = 64
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

type cb_buscarepa from commandbutton within w_info_repalletizado_lineal_compal
integer x = 1349
integer y = 1628
integer width = 91
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "..."
end type

event clicked;istr_busq.argum[1]	=	String(uo_SelCliente.Codigo)
istr_busq.argum[2]	=	String(uo_SelPlantas.Codigo)
istr_busq.argum[3]	=	'1'

OpenWithParm(w_busc_repalletenca, istr_busq)

istr_busq	       = Message.PowerObjectParm

IF istr_busq.argum[5] <> "" THEN
	em_numero.Text				= istr_busq.argum[5]
	istr_mant.argumento[2]	= istr_busq.argum[5]
	cbx_etiq.Enabled = TRUE
ELSE
	em_numero.SetFocus()
	cbx_etiq.Enabled = FALSE
END IF
end event

type cbx_etiq from checkbox within w_info_repalletizado_lineal_compal
integer x = 1550
integer y = 1636
integer width = 480
integer height = 80
integer taborder = 60
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
string text = "Con Etiquetas"
end type

event clicked;If THIS.Checked then
	ii_etiq = 1
ELSE
	ii_etiq = 0
END IF
end event

type st_5 from statictext within w_info_repalletizado_lineal_compal
integer x = 256
integer y = 1516
integer width = 1902
integer height = 312
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_8 from statictext within w_info_repalletizado_lineal_compal
integer x = 306
integer y = 792
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
long backcolor = 553648127
string text = "Productor"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_repalletizado_lineal_compal
integer x = 306
integer y = 960
integer width = 270
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Especie"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_variedad from statictext within w_info_repalletizado_lineal_compal
integer x = 306
integer y = 1152
integer width = 279
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Variedad"
boolean focusrectangle = false
end type

type st_12 from statictext within w_info_repalletizado_lineal_compal
integer x = 306
integer y = 1332
integer width = 256
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
boolean enabled = false
string text = "Calidad"
boolean focusrectangle = false
end type

type em_calidad from editmask within w_info_repalletizado_lineal_compal
integer x = 727
integer y = 1332
integer width = 261
integer height = 96
integer taborder = 130
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean enabled = false
alignment alignment = center!
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string mask = "xxx"
end type

event modified;Integer	li_especie, li_variedad, li_cantid
String	ls_calibre

IF This.Text <> '' THEN
	li_especie	=	Integer(istr_mant.argumento[8]) // Especie
	li_variedad	=	Integer(istr_mant.argumento[10]) // Variedad
	ls_calibre	=	This.Text
	
	IF NOT iuo_calibre.existe(li_especie,li_variedad,ls_calibre,True,sqlca) THEN
		em_calidad.Text = ''
		This.SetFocus()
		Return 1
	ELSE	
		istr_mant.argumento[11]	 = iuo_calibre.calibre
		em_calidad.Text			 = iuo_calibre.calibre
		Return 1
	END IF	
END IF	

end event

type cbx_calidad from checkbox within w_info_repalletizado_lineal_compal
integer x = 727
integer y = 1248
integer width = 297
integer height = 80
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_calidad.Text			=	''
	em_calidad.Enabled		=	False
	istr_mant.argumento[11]	=	'*'
ELSE
	em_calidad.Enabled		=	True
	em_calidad.SetFocus()
END IF


end event

type st_embalaje from statictext within w_info_repalletizado_lineal_compal
integer x = 1193
integer y = 1348
integer width = 288
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
boolean enabled = false
string text = "Embalaje"
boolean focusrectangle = false
end type

type cbx_embalaje from checkbox within w_info_repalletizado_lineal_compal
integer x = 1600
integer y = 1248
integer width = 402
integer height = 80
integer taborder = 220
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_embalaje.Enabled			=	False
	cb_buscaembalaje.Enabled	=	False
	em_embalaje.Text				=	''
ELSE
	em_embalaje.Enabled			=	True
	cb_buscaembalaje.Enabled	=	True
END IF
end event

type em_embalaje from editmask within w_info_repalletizado_lineal_compal
integer x = 1600
integer y = 1332
integer width = 261
integer height = 96
integer taborder = 240
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean enabled = false
alignment alignment = center!
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string mask = "xxxxxxxxxx"
end type

event modified;Integer		li_cliente
String		ls_embalaje, ls_Nombre

li_cliente	=	Integer(istr_mant.argumento[1]) // Cliente
ls_embalaje	=	This.Text

SELECT	emba_nombre
	INTO	:ls_Nombre
	FROM	dbo.embalajesprod
	WHERE	clie_codigo	=	:li_cliente
	AND	emba_codigo	=	:ls_embalaje;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Embalajes")
	This.SetFocus()
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Embalaje no ha sido Definido.~r~r" + &
		"Ingrese o seleccione otro Código.")
	This.SetFocus()
END IF

end event

type cb_buscaembalaje from commandbutton within w_info_repalletizado_lineal_compal
integer x = 1879
integer y = 1336
integer width = 96
integer height = 84
integer taborder = 250
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean enabled = false
string text = "..."
end type

event clicked;
Str_busqueda	lstr_busq

lstr_busq.argum[1]	=	istr_mant.argumento[1] // Cliente

OpenWithParm(w_busc_embalajesprod, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[2] = "" THEN
	em_embalaje.SetFocus()
ELSE
	em_embalaje.Text			=	lstr_busq.argum[2]
END IF
end event

type uo_selcliente from uo_seleccion_clientesprod within w_info_repalletizado_lineal_compal
event destroy ( )
integer x = 727
integer y = 460
integer height = 92
integer taborder = 20
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplantas from uo_seleccion_plantas within w_info_repalletizado_lineal_compal
event destroy ( )
integer x = 727
integer y = 588
integer height = 92
integer taborder = 20
boolean bringtotop = true
end type

on uo_selplantas.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selespecie from uo_seleccion_especie within w_info_repalletizado_lineal_compal
event destroy ( )
integer x = 722
integer y = 868
integer height = 164
integer taborder = 90
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

type uo_selvariedad from uo_seleccion_variedad within w_info_repalletizado_lineal_compal
integer x = 727
integer y = 1052
integer taborder = 100
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type uo_selproductor from uo_seleccion_productor within w_info_repalletizado_lineal_compal
event destroy ( )
integer x = 727
integer y = 696
integer taborder = 50
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor::destroy
end on

