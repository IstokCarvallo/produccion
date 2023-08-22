$PBExportHeader$w_info_repalletdeta_listado.srw
$PBExportComments$Informe de Revisión Repalletizajes.
forward
global type w_info_repalletdeta_listado from w_para_informes
end type
type st_4 from statictext within w_info_repalletdeta_listado
end type
type st_1 from statictext within w_info_repalletdeta_listado
end type
type st_5 from statictext within w_info_repalletdeta_listado
end type
type st_2 from statictext within w_info_repalletdeta_listado
end type
type em_numero from editmask within w_info_repalletdeta_listado
end type
type st_6 from statictext within w_info_repalletdeta_listado
end type
type cb_buscarepa from commandbutton within w_info_repalletdeta_listado
end type
type uo_selplantas from uo_seleccion_plantas within w_info_repalletdeta_listado
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_repalletdeta_listado
end type
end forward

global type w_info_repalletdeta_listado from w_para_informes
integer x = 14
integer y = 32
integer width = 2688
integer height = 1344
string title = "INFORME REPALLETIZADO"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_4 st_4
st_1 st_1
st_5 st_5
st_2 st_2
em_numero em_numero
st_6 st_6
cb_buscarepa cb_buscarepa
uo_selplantas uo_selplantas
uo_selcliente uo_selcliente
end type
global w_info_repalletdeta_listado w_info_repalletdeta_listado

type variables
str_busqueda istr_busq
str_mant istr_mant

Integer ii_TipoRepa
end variables

forward prototypes
public function boolean noexistefolio (long al_numero)
public function boolean existenumero ()
end prototypes

public function boolean noexistefolio (long al_numero);Integer	li_TipoRepa
Long		ll_Numero,ll_CantTarjas,ll_CantParticipa,ll_CantInspec
Date		ld_FechaRepa

ll_Numero	=	Long(em_numero.Text)

SELECT	repe_fecrep, repe_tipopa
	INTO	:ld_FechaRepa, :li_TipoRepa
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

public function boolean existenumero ();Integer	li_TipoRepa
Long		ll_Numero

ll_Numero	=	Long(Istr_mant.Argumento[2])


SELECT	repe_tipopa
INTO	:ii_TipoRepa
FROM	dbo.repalletenca
WHERE	clie_codigo	=	:uo_SelCliente.Codigo
AND	plde_codigo	=	:uo_SelPlantas.Codigo
AND	repe_numero	=	:ll_Numero;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Detalle de Repalletizajes")
		
		RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Número de Repalletizado no ha sido Ingresado.~r~r" + &
				"Ingrese o seleccione otro Número.", Exclamation!, Ok!)
		RETURN True		
END IF	

Return False
end function

on w_info_repalletdeta_listado.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.st_5=create st_5
this.st_2=create st_2
this.em_numero=create em_numero
this.st_6=create st_6
this.cb_buscarepa=create cb_buscarepa
this.uo_selplantas=create uo_selplantas
this.uo_selcliente=create uo_selcliente
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_5
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.em_numero
this.Control[iCurrent+6]=this.st_6
this.Control[iCurrent+7]=this.cb_buscarepa
this.Control[iCurrent+8]=this.uo_selplantas
this.Control[iCurrent+9]=this.uo_selcliente
end on

on w_info_repalletdeta_listado.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.st_5)
destroy(this.st_2)
destroy(this.em_numero)
destroy(this.st_6)
destroy(this.cb_buscarepa)
destroy(this.uo_selplantas)
destroy(this.uo_selcliente)
end on

event open;call super::open;Boolean lb_Cerrar 

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlantas.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlantas.Seleccion(False, False)
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlantas.Inicia(gi_CodPlanta)
	
	istr_mant.argumento[2]	=	"-1"
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_repalletdeta_listado
end type

type st_computador from w_para_informes`st_computador within w_info_repalletdeta_listado
end type

type st_usuario from w_para_informes`st_usuario within w_info_repalletdeta_listado
end type

type st_temporada from w_para_informes`st_temporada within w_info_repalletdeta_listado
end type

type p_logo from w_para_informes`p_logo within w_info_repalletdeta_listado
end type

type st_titulo from w_para_informes`st_titulo within w_info_repalletdeta_listado
integer y = 296
integer width = 1902
string text = "Informe de Cuadratura Repalletizado"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_repalletdeta_listado
string tag = "Imprimir Reporte"
integer x = 2373
integer y = 556
integer taborder = 50
end type

event pb_acepta::clicked;SetPointer(Arrow!)
Integer	fila,li_cbx_planta,li_especie,li_numero
Long		li_planta,li_cliente

istr_info.titulo	= 'INFORME DE CUADRATURA REPALLETIZADO'

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_repalletdeta_listado"
vinf.dw_1.SetTransObject(sqlca)

IF em_numero.text ="" THEN istr_mant.argumento[2] = '-1'

fila	=	vinf.dw_1.Retrieve(uo_SelPlantas.Codigo, Long(istr_mant.Argumento[2]), uo_SelCliente.Codigo,ii_TipoRepa)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)

ELSE
	F_Membrete(vinf.dw_1)
	
	istr_mant.Argumento[6] = 'Completar Pallet'
	istr_mant.Argumento[7] = 'Reembalaje'
	istr_mant.Argumento[8] = 'Cambio de Altura'
	istr_mant.Argumento[9] = 'Cambio de Altura'
	
	IF ii_TipoRepa = 3 THEN
		vinf.dw_1.Modify("tipopa.text = '" + istr_mant.Argumento[6] + "'")
	ELSEIF ii_TipoRepa = 7 THEN 
		vinf.dw_1.Modify("tipopa.text = '" + istr_mant.Argumento[7] + "'")
	ELSEIF ii_TipoRepa = 1 THEN 
		vinf.dw_1.Modify("tipopa.text = '" + istr_mant.Argumento[8] + "'")	
	ELSEIF ii_TipoRepa = 2 THEN 
		vinf.dw_1.Modify("tipopa.text = '" + istr_mant.Argumento[9] + "'")	
	END IF	
	
	vinf.dw_1.Modify("planta.text = '" + uo_SelPlantas.Nombre + "'")
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_repalletdeta_listado
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2382
integer taborder = 60
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_info_repalletdeta_listado
integer x = 251
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

type st_1 from statictext within w_info_repalletdeta_listado
integer x = 361
integer y = 628
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

type st_5 from statictext within w_info_repalletdeta_listado
integer x = 251
integer y = 780
integer width = 1902
integer height = 236
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

type st_2 from statictext within w_info_repalletdeta_listado
integer x = 370
integer y = 852
integer width = 517
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
string text = "Nro Repaletizado"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_numero from editmask within w_info_repalletdeta_listado
integer x = 914
integer y = 840
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
	Istr_mant.Argumento[2] = This.Text
	IF existenumero() THEN
		This.Text= ""
		Return 1
	ELSE	
		IF NoExisteFolio(Long(This.Text)) THEN
			This.Text	=	""
			
			This.SetFocus()
		ELSE
			istr_mant.argumento[2]	=	String(Long(This.Text), '00000000')
		END IF
	END IF	
END IF

end event

type st_6 from statictext within w_info_repalletdeta_listado
integer x = 361
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

type cb_buscarepa from commandbutton within w_info_repalletdeta_listado
integer x = 1358
integer y = 844
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

event clicked;istr_busq.argum[1]	=	String(uo_SelCliente.Codigo)
istr_busq.argum[2]	=	String(uo_SelPlantas.Codigo)
istr_busq.argum[3]	=	'1'

OpenWithParm(w_busc_repalletenca, istr_busq)

istr_busq	       = Message.PowerObjectParm

istr_mant.argumento[2] = istr_busq.argum[5]
IF existenumero() THEN
	em_numero.Text = ""
	Return 1
ELSE	
	IF istr_busq.argum[5] <> "" THEN
		em_numero.Text				= istr_busq.argum[5]
		istr_mant.argumento[2]	= istr_busq.argum[5]
	ELSE
		em_numero.SetFocus()
	END IF
END IF	
end event

type uo_selplantas from uo_seleccion_plantas within w_info_repalletdeta_listado
event destroy ( )
integer x = 919
integer y = 616
integer height = 96
integer taborder = 20
boolean bringtotop = true
end type

on uo_selplantas.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selcliente from uo_seleccion_clientesprod within w_info_repalletdeta_listado
event destroy ( )
integer x = 919
integer y = 496
integer height = 96
integer taborder = 20
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

