$PBExportHeader$w_info_repalletizado_lineal.srw
$PBExportComments$Informe de Repalletizajes.
forward
global type w_info_repalletizado_lineal from w_para_informes
end type
type gb_3 from groupbox within w_info_repalletizado_lineal
end type
type st_4 from statictext within w_info_repalletizado_lineal
end type
type st_1 from statictext within w_info_repalletizado_lineal
end type
type st_2 from statictext within w_info_repalletizado_lineal
end type
type em_numero from editmask within w_info_repalletizado_lineal
end type
type dw_2 from datawindow within w_info_repalletizado_lineal
end type
type st_6 from statictext within w_info_repalletizado_lineal
end type
type dw_1 from datawindow within w_info_repalletizado_lineal
end type
type cb_buscarepa from commandbutton within w_info_repalletizado_lineal
end type
type cbx_etiq from checkbox within w_info_repalletizado_lineal
end type
type st_7 from statictext within w_info_repalletizado_lineal
end type
type em_cambio from editmask within w_info_repalletizado_lineal
end type
type cb_cambio from uo_buscar within w_info_repalletizado_lineal
end type
type st_5 from statictext within w_info_repalletizado_lineal
end type
end forward

global type w_info_repalletizado_lineal from w_para_informes
integer x = 14
integer y = 32
integer width = 2725
integer height = 1420
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
dw_2 dw_2
st_6 st_6
dw_1 dw_1
cb_buscarepa cb_buscarepa
cbx_etiq cbx_etiq
st_7 st_7
em_cambio em_cambio
cb_cambio cb_cambio
st_5 st_5
end type
global w_info_repalletizado_lineal w_info_repalletizado_lineal

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta,idwc_especie

Integer ii_etiq
end variables

forward prototypes
public function boolean noexistefolioaltura (long al_numero)
public function boolean noexistefolio (long al_numero)
end prototypes

public function boolean noexistefolioaltura (long al_numero);Integer	li_Cliente, li_Planta
Long		ll_existe,ll_Numero, li_cantidad, ll_cantpalcambio, ll_cantparticipacambio, ll_cantinspeccambio

li_Cliente	=	dw_2.Object.clie_codigo[1]
li_Planta	=	dw_1.Object.plde_codigo[1]
ll_Numero	=	Long(em_cambio.Text)


SELECT	altu_numero
	INTO	:ll_existe
	FROM	dbo.ALPALLETENCAB
	WHERE	clie_codigo	=	:li_Cliente
	AND	plde_codigo	=	:li_Planta
	AND	altu_numero	=	:al_Numero ;
			
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla AlPalletencab")
	
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Número de Cambio Altura no ha sido Ingresado.~r~r" + &
					"Ingrese o seleccione otro Número.", Exclamation!, Ok!)
					
	RETURN True
ELSE
	SELECT	Count(paen_numero)
		INTO	:ll_cantpalcambio
		FROM	dbo.ALPALLETFRUTA
		WHERE	clie_codigo	=	:li_Cliente
		AND	plde_codigo	=	:li_Planta
		AND	altu_numero	=	:ll_Numero;
				
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Detalle de Repalletizajes")
			
			RETURN True
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Error", "Número de Cambio de Altura no tiene Detalle.~r~r" + &
						"Consulte con Encargado.", Exclamation!, Ok!)
			
			RETURN True
	
	END IF

	RETURN False
END IF
end function

public function boolean noexistefolio (long al_numero);Integer	li_Cliente, li_Planta, li_TipoRepa
Long		ll_Numero,ll_CantTarjas,ll_CantParticipa,ll_CantInspec
Date		ld_FechaRepa

li_Cliente	=	dw_2.Object.clie_codigo[1]
li_Planta	=	dw_1.Object.plde_codigo[1]
ll_Numero	=	Long(em_numero.Text)

SELECT	repe_fecrep, repe_tipopa
	INTO	:ld_FechaRepa, :li_TipoRepa
	FROM	dbo.REPALLETENCA
	WHERE	clie_codigo	=	:li_Cliente
	AND	plde_codigo	=	:li_Planta
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
		WHERE	clie_codigo	=	:li_Cliente
		AND	plde_codigo	=	:li_Planta
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
			WHERE	clie_codigo	=	:li_Cliente
			AND	plde_codigo	=	:li_Planta
			AND	repe_numero	=	:ll_Numero;
		
		SELECT	Count(rep.paen_numero)
			INTO	:ll_CantInspec
			FROM	dbo.REPALLETDETA as rep, dbo.PALLETENCAB as pae
			WHERE	rep.clie_codigo	=	:li_Cliente
			AND	rep.plde_codigo	=	:li_Planta
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

on w_info_repalletizado_lineal.create
int iCurrent
call super::create
this.gb_3=create gb_3
this.st_4=create st_4
this.st_1=create st_1
this.st_2=create st_2
this.em_numero=create em_numero
this.dw_2=create dw_2
this.st_6=create st_6
this.dw_1=create dw_1
this.cb_buscarepa=create cb_buscarepa
this.cbx_etiq=create cbx_etiq
this.st_7=create st_7
this.em_cambio=create em_cambio
this.cb_cambio=create cb_cambio
this.st_5=create st_5
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_3
this.Control[iCurrent+2]=this.st_4
this.Control[iCurrent+3]=this.st_1
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.em_numero
this.Control[iCurrent+6]=this.dw_2
this.Control[iCurrent+7]=this.st_6
this.Control[iCurrent+8]=this.dw_1
this.Control[iCurrent+9]=this.cb_buscarepa
this.Control[iCurrent+10]=this.cbx_etiq
this.Control[iCurrent+11]=this.st_7
this.Control[iCurrent+12]=this.em_cambio
this.Control[iCurrent+13]=this.cb_cambio
this.Control[iCurrent+14]=this.st_5
end on

on w_info_repalletizado_lineal.destroy
call super::destroy
destroy(this.gb_3)
destroy(this.st_4)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.em_numero)
destroy(this.dw_2)
destroy(this.st_6)
destroy(this.dw_1)
destroy(this.cb_buscarepa)
destroy(this.cbx_etiq)
destroy(this.st_7)
destroy(this.em_cambio)
destroy(this.cb_cambio)
destroy(this.st_5)
end on

event open;call super::open;String	ls_Planta

SELECT	plde_nombre
	INTO	:ls_Planta
	FROM	dbo.plantadesp
	WHERE	plde_codigo	=	:gi_CodPlanta ;

dw_2.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_2.InsertRow(0)
dw_2.SetItem(1, "clie_codigo", gi_CodExport)

dw_1.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_1.InsertRow(0)
dw_1.SetItem(1, "plde_codigo", gi_CodPlanta)

istr_mant.argumento[1]	=	String(gi_codplanta)
istr_mant.argumento[2]	=	""
istr_mant.argumento[3]	=	String(gi_codexport)
istr_mant.argumento[4]	=	String(gi_CodEspecie)
istr_mant.argumento[5]	=	ls_Planta
end event

type pb_excel from w_para_informes`pb_excel within w_info_repalletizado_lineal
end type

type st_computador from w_para_informes`st_computador within w_info_repalletizado_lineal
end type

type st_usuario from w_para_informes`st_usuario within w_info_repalletizado_lineal
end type

type st_temporada from w_para_informes`st_temporada within w_info_repalletizado_lineal
end type

type p_logo from w_para_informes`p_logo within w_info_repalletizado_lineal
end type

type st_titulo from w_para_informes`st_titulo within w_info_repalletizado_lineal
integer y = 296
integer width = 1902
string text = "Informe de Cuadratura Repalletizado"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_repalletizado_lineal
string tag = "Imprimir Reporte"
integer x = 2350
integer y = 552
integer taborder = 50
end type

event pb_acepta::clicked;SetPointer(Arrow!)
Integer	fila,li_cbx_planta,li_especie,li_numero
Long		li_planta,li_cliente

istr_info.titulo	= 'INFORME DE CUADRATURA REPALLETIZADO'

OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_info_repaletizado_lineal"

vinf.dw_1.SetTransObject(sqlca)

IF em_numero.text ="" THEN
	istr_mant.argumento[2] = '-1'
END IF
IF em_cambio.text ="" THEN
	istr_mant.argumento[13] = '-1'
END IF

	fila	=	vinf.dw_1.Retrieve(Integer(istr_mant.Argumento[1]), &
										Long(istr_mant.Argumento[2]), &
										Integer(istr_mant.Argumento[3]),ii_etiq,&
										Long(istr_mant.Argumento[13]))
	
	IF fila = -1 THEN
		MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
						"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
	
	ELSEIF fila = 0 THEN
		MessageBox( "No Existe información", "No existe información para este informe.", &
						 StopSign!, Ok!)
	
	ELSE
			F_Membrete(vinf.dw_1)
			
			vinf.dw_1.Modify("planta.text = '" + istr_mant.Argumento[5] + "'")
			vinf.dw_1.Modify("altura.text = '" + istr_mant.Argumento[13] + "'")
			IF gs_Ambiente <> 'Windows' THEN
				F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
			END IF
	END IF

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_repalletizado_lineal
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2345
integer taborder = 70
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type gb_3 from groupbox within w_info_repalletizado_lineal
integer x = 1486
integer y = 816
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

type st_4 from statictext within w_info_repalletizado_lineal
integer x = 247
integer y = 440
integer width = 1902
integer height = 340
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

type st_1 from statictext within w_info_repalletizado_lineal
integer x = 343
integer y = 636
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

type st_2 from statictext within w_info_repalletizado_lineal
integer x = 352
integer y = 856
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

type em_numero from editmask within w_info_repalletizado_lineal
integer x = 901
integer y = 844
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
		istr_mant.argumento[2]	=	String(Long(This.Text), '00000')
	END IF
END IF

IF this.text <> '' and NOT isnull(This.text) THEN cbx_etiq.enabled = true
end event

type dw_2 from datawindow within w_info_repalletizado_lineal
integer x = 850
integer y = 504
integer width = 1230
integer height = 96
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]
Integer	li_planta	

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[3]	=	data
ELSE
	This.SetItem(1, "clie_codigo", gi_CodExport)
	
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_repalletizado_lineal
integer x = 343
integer y = 512
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

type dw_1 from datawindow within w_info_repalletizado_lineal
integer x = 850
integer y = 624
integer width = 969
integer height = 92
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[], ls_planta
Integer	li_Cliente, li_planta

li_Cliente	=	dw_2.Object.clie_codigo[1]

IF ExistePlanta(li_Cliente, Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[1]	=	data
	li_planta = integer(data)
	
	select plde_nombre into :ls_planta
	from dba.plantadesp
	where plde_codigo = :li_planta;
	
	istr_mant.Argumento[5]	=	ls_planta
ELSE
	This.SetItem(1, "plde_codigo", gi_CodPlanta)
	
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type cb_buscarepa from commandbutton within w_info_repalletizado_lineal
integer x = 1339
integer y = 848
integer width = 91
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "..."
end type

event clicked;istr_busq.argum[1]	=	String(dw_2.Object.clie_codigo[1])
istr_busq.argum[2]	=	String(dw_1.Object.plde_codigo[1])
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

type cbx_etiq from checkbox within w_info_repalletizado_lineal
integer x = 1541
integer y = 928
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

type st_7 from statictext within w_info_repalletizado_lineal
integer x = 347
integer y = 1012
integer width = 549
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
boolean enabled = false
string text = "Nro.Cambio Altura"
boolean focusrectangle = false
end type

type em_cambio from editmask within w_info_repalletizado_lineal
event getfocus pbm_ensetfocus
integer x = 901
integer y = 984
integer width = 393
integer height = 92
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string mask = "#####"
string displaydata = "$"
end type

event modified;IF This.Text <> "" THEN
	IF NoExisteFolioAltura(Long(This.Text)) THEN
		This.Text	=	""
		cbx_etiq.enabled = FALSE
		This.SetFocus()
	ELSE
		istr_mant.argumento[13]	=	String(Long(This.Text), '00000')
      cbx_etiq.enabled = TRUE
	END IF
END IF
end event

type cb_cambio from uo_buscar within w_info_repalletizado_lineal
integer x = 1339
integer y = 988
integer width = 96
integer height = 88
integer taborder = 0
boolean bringtotop = true
end type

event clicked;istr_busq.argum[1]	=	String(dw_2.Object.clie_codigo[1])
istr_busq.argum[2]	=	String(dw_1.Object.plde_codigo[1])
istr_busq.argum[3]	=	'1'

OpenWithParm(w_busc_cambioaltura, istr_busq)

istr_busq	       = Message.PowerObjectParm

IF istr_busq.argum[5] <> "" THEN
	em_cambio.Text				= istr_busq.argum[5]
	istr_mant.argumento[13]	= istr_busq.argum[5]
	IF em_numero.text <> '' OR isnull(em_numero.text) THEN
		cbx_etiq.enabled = TRUE
	ELSE
		cbx_etiq.enabled = FALSE
	END IF
ELSE
	cbx_etiq.enabled = FALSE
	em_cambio.SetFocus()
END IF
end event

type st_5 from statictext within w_info_repalletizado_lineal
integer x = 247
integer y = 784
integer width = 1902
integer height = 412
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

