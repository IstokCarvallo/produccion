$PBExportHeader$w_info_palletprefrio.srw
$PBExportComments$Informe Consolidado General de Embarques.
forward
global type w_info_palletprefrio from w_para_informes
end type
type st_4 from statictext within w_info_palletprefrio
end type
type st_3 from statictext within w_info_palletprefrio
end type
type st_1 from statictext within w_info_palletprefrio
end type
type em_desde from editmask within w_info_palletprefrio
end type
type st_2 from statictext within w_info_palletprefrio
end type
type em_hasta from editmask within w_info_palletprefrio
end type
type st_5 from statictext within w_info_palletprefrio
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_palletprefrio
end type
type uo_selplanta from uo_seleccion_plantas within w_info_palletprefrio
end type
type st_6 from statictext within w_info_palletprefrio
end type
type cbx_numero from checkbox within w_info_palletprefrio
end type
type em_numero from editmask within w_info_palletprefrio
end type
type st_8 from statictext within w_info_palletprefrio
end type
type cb_buscar from commandbutton within w_info_palletprefrio
end type
type cbx_fecha from checkbox within w_info_palletprefrio
end type
type uo_selcamara from uo_seleccion_camarasbode within w_info_palletprefrio
end type
end forward

global type w_info_palletprefrio from w_para_informes
integer width = 2318
integer height = 1620
event ue_listo ( )
st_4 st_4
st_3 st_3
st_1 st_1
em_desde em_desde
st_2 st_2
em_hasta em_hasta
st_5 st_5
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
st_6 st_6
cbx_numero cbx_numero
em_numero em_numero
st_8 st_8
cb_buscar cb_buscar
cbx_fecha cbx_fecha
uo_selcamara uo_selcamara
end type
global w_info_palletprefrio w_info_palletprefrio

type variables
str_mant istr_mant
DataWindowChild 		idwc_cliente, idwc_especie, idwc_embarque,idwc_operaciones,&
                     idwc_planta, idwc_productor, idwc_tipocamion, idwc_tiposag

Integer	ii_Cliente, ii_Especie, ii_Planta, ii_Operacion, ii_agrupa,ii_tipocamion
String	is_Embarque, is_NomEspecie, is_NomEmbarque, is_NomNave, is_NomPlanta, &
			is_NomCliente, is_OPeracion
Long		ii_productor

Date		id_FechaZarpe, id_FechaAcceso
Time		it_HoraAcceso


end variables

forward prototypes
public function boolean existeproductor (long productor)
public function string buscaregionplanta (integer planta)
public function integer buscaplantasag (integer planta)
public function boolean existeplanilla (long al_planilla)
end prototypes

event ue_listo();w_main.SetMicroHelp("Listo")
SetPointer(Arrow!)

end event

public function boolean existeproductor (long productor);String	ls_Nombre

SELECT	prod_nombre
	INTO	:ls_Nombre
	FROM	dbo.productores
	WHERE	prod_codigo	=	:Productor ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Productor")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Productor no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	ii_productor = Productor
	RETURN True
END IF
end function

public function string buscaregionplanta (integer planta);Integer	li_region
String	ls_region

ls_region	=	'0'

SELECT plde_region
INTO	:li_region
FROM dbo.PLANTADESP
WHERE	plde_codigo=:Planta;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Plantadesp")
	
	RETURN ls_region
	
ELSEIF sqlca.SQLCode = 100 THEN
	RETURN ls_region
END IF	

ls_region	=	String(li_region)

IF li_region = 13 THEN
	RETURN "M"
ELSE
	RETURN ls_region
END IF

end function

public function integer buscaplantasag (integer planta);Long	li_codmul

SELECT plde_codmul
INTO	:li_codmul
FROM dbo.PLANTADESP
WHERE	plde_codigo=:Planta;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Plantadesp")
	
	RETURN li_codmul
	
ELSEIF sqlca.SQLCode = 100 THEN
	RETURN li_codmul
END IF	

RETURN li_codmul

end function

public function boolean existeplanilla (long al_planilla);Integer	li_codexp, li_planta
Date		ld_fecha

li_codexp		=	Integer(istr_mant.argumento[1])
li_planta		=	Integer(istr_mant.argumento[2])

IF (al_planilla <> 0) OR li_planta = 0 THEN
	
	SELECT Min(defe_fecdes)
		INTO	:ld_fecha
		FROM	dbo.DESPAFRIGOEN 
		WHERE	plde_codigo =	:li_planta
		AND	clie_codigo	=	:li_codexp
		AND	defe_plasag	=	:al_planilla ;
				
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla DESPAFRIGOEN")

		RETURN False
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "No existe Planilla S.A.G. Indicado.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		pb_acepta.Enabled	= False
		RETURN False
	ELSEIF IsNull(ld_fecha) THEN
					MessageBox("Atención", "No existe Planilla S.A.G. Indicado.~r~rIngrese otro Número.", &
									Exclamation!, Ok!)
					pb_acepta.Enabled	= False
					RETURN False
		 ELSE
					pb_acepta.Enabled	= True
					RETURN True
		 END IF
ELSE
	MessageBox("Atención", "Faltan parámetros de búsqueda.~r~rIngreselos todos.", &
					Exclamation!, Ok!)
	RETURN False
END IF
end function

on w_info_palletprefrio.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_3=create st_3
this.st_1=create st_1
this.em_desde=create em_desde
this.st_2=create st_2
this.em_hasta=create em_hasta
this.st_5=create st_5
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
this.st_6=create st_6
this.cbx_numero=create cbx_numero
this.em_numero=create em_numero
this.st_8=create st_8
this.cb_buscar=create cb_buscar
this.cbx_fecha=create cbx_fecha
this.uo_selcamara=create uo_selcamara
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_3
this.Control[iCurrent+3]=this.st_1
this.Control[iCurrent+4]=this.em_desde
this.Control[iCurrent+5]=this.st_2
this.Control[iCurrent+6]=this.em_hasta
this.Control[iCurrent+7]=this.st_5
this.Control[iCurrent+8]=this.uo_selcliente
this.Control[iCurrent+9]=this.uo_selplanta
this.Control[iCurrent+10]=this.st_6
this.Control[iCurrent+11]=this.cbx_numero
this.Control[iCurrent+12]=this.em_numero
this.Control[iCurrent+13]=this.st_8
this.Control[iCurrent+14]=this.cb_buscar
this.Control[iCurrent+15]=this.cbx_fecha
this.Control[iCurrent+16]=this.uo_selcamara
end on

on w_info_palletprefrio.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_3)
destroy(this.st_1)
destroy(this.em_desde)
destroy(this.st_2)
destroy(this.em_hasta)
destroy(this.st_5)
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
destroy(this.st_6)
destroy(this.cbx_numero)
destroy(this.em_numero)
destroy(this.st_8)
destroy(this.cb_buscar)
destroy(this.cbx_fecha)
destroy(this.uo_selcamara)
end on

event open;call super::open;Boolean	lb_Cerrar

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True
If IsNull(uo_selCamara.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(True, False)
	uo_selCamara.Seleccion(True, False)
	uo_SelPlanta.Todos(False)

	uo_SelCliente.Codigo = gi_codexport
	uo_SelCliente.dw_Seleccion.Object.Codigo[1] = gi_codexport
	
	uo_SelPlanta.Codigo = gi_codplanta
	uo_SelPlanta.dw_Seleccion.Object.Codigo[1] = gi_codplanta
	
	uo_selCamara.Filtra(uo_SelPlanta.Codigo)
	
	em_desde.Text				=	String(RelativeDate(today(), -365))
	em_hasta.Text				=	String(Today())
	
	GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)
End If
end event

event close;call super::close;GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)
end event

type pb_excel from w_para_informes`pb_excel within w_info_palletprefrio
end type

type st_computador from w_para_informes`st_computador within w_info_palletprefrio
end type

type st_usuario from w_para_informes`st_usuario within w_info_palletprefrio
end type

type st_temporada from w_para_informes`st_temporada within w_info_palletprefrio
fontfamily fontfamily = swiss!
string facename = "Tahoma"
end type

type p_logo from w_para_informes`p_logo within w_info_palletprefrio
end type

type st_titulo from w_para_informes`st_titulo within w_info_palletprefrio
integer width = 1559
integer textsize = -8
string text = "Tiempos de Fruta Prefrio"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_palletprefrio
string tag = "Imprimir Reporte"
integer x = 1961
integer y = 444
integer taborder = 110
integer textsize = -8
fontcharset fontcharset = ansi!
string facename = "Tahoma"
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Long	ll_Fila, ll_Numero = -1
Date	ld_Desde, ld_Hasta

istr_info.titulo	= 'INFORME PALLETS EN PREFRIO'

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_palletprefrio"
vinf.dw_1.SetTransObject(sqlca)

If cbx_Fecha.Checked Then
	ld_Desde =	Date(Date('19000101'))
	ld_Hasta	=	Date(Today())
Else
	ld_Desde	=	Date(em_Desde.Text)
	ld_Hasta	=	Date(em_Hasta.Text)
End If

If Not cbx_numero.Checked Then ll_Numero = Long(em_Numero.Text)

ll_fila	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, ll_Numero, uo_selCamara.Codigo, ld_Desde, ld_Hasta)

If ll_fila	 = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_fila	= 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)	
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_palletprefrio
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 1961
integer y = 732
integer taborder = 120
integer textsize = -8
fontcharset fontcharset = ansi!
string facename = "Tahoma"
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_info_palletprefrio
integer x = 343
integer y = 960
integer width = 247
integer height = 64
integer taborder = 40
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Camara"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_palletprefrio
integer x = 343
integer y = 488
integer width = 229
integer height = 64
integer taborder = 20
boolean bringtotop = true
integer textsize = -8
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

type st_1 from statictext within w_info_palletprefrio
integer x = 247
integer y = 408
integer width = 1559
integer height = 1048
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_palletprefrio
integer x = 594
integer y = 1312
integer width = 425
integer height = 100
integer taborder = 20
boolean bringtotop = true
integer textsize = -8
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

type st_2 from statictext within w_info_palletprefrio
integer x = 343
integer y = 1328
integer width = 229
integer height = 64
integer taborder = 40
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Desde"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_palletprefrio
integer x = 1280
integer y = 1312
integer width = 425
integer height = 100
integer taborder = 30
boolean bringtotop = true
integer textsize = -8
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

type st_5 from statictext within w_info_palletprefrio
integer x = 1047
integer y = 1332
integer width = 229
integer height = 60
integer taborder = 40
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Hasta"
boolean focusrectangle = false
end type

type uo_selcliente from uo_seleccion_clientesprod within w_info_palletprefrio
event destroy ( )
integer x = 603
integer y = 476
integer height = 88
integer taborder = 50
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_info_palletprefrio
event destroy ( )
integer x = 603
integer y = 636
integer taborder = 50
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		uo_selCamara.Filtra(0)
		
	Case Else
		uo_selCamara.Filtra(This.Codigo)
		
End Choose
end event

type st_6 from statictext within w_info_palletprefrio
integer x = 343
integer y = 732
integer width = 229
integer height = 64
integer taborder = 40
boolean bringtotop = true
integer textsize = -8
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

type cbx_numero from checkbox within w_info_palletprefrio
integer x = 1275
integer y = 1084
integer width = 306
integer height = 80
boolean bringtotop = true
integer textsize = -8
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

event clicked;If This.Checked Then
	cb_buscar.Enabled = False
	em_numero.Enabled = False
	em_numero.Text = ''
Else
	cb_buscar.Enabled = True
	em_numero.Enabled = True
	em_numero.Text = ''
End If
end event

type em_numero from editmask within w_info_palletprefrio
integer x = 603
integer y = 1072
integer width = 494
integer height = 112
integer taborder = 60
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean enabled = false
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

type st_8 from statictext within w_info_palletprefrio
integer x = 343
integer y = 1096
integer width = 247
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Pallet"
boolean focusrectangle = false
end type

type cb_buscar from commandbutton within w_info_palletprefrio
integer x = 1129
integer y = 1076
integer width = 119
integer height = 100
integer taborder = 70
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean enabled = false
string text = "..."
end type

event clicked;str_busqueda	lstr_Busq

lstr_busq.argum[1]	=	String(uo_SelCliente.Codigo)
lstr_busq.argum[5]   = ''

OpenWithParm(w_busc_palletencab, lstr_busq)

lstr_busq = Message.PowerObjectParm

if UpperBound(lstr_busq.Argum) < 2 Then Return
If lstr_busq.Argum[2] <> "" Then	em_numero.Text 			=	lstr_busq.argum[2]

end event

type cbx_fecha from checkbox within w_info_palletprefrio
integer x = 599
integer y = 1200
integer width = 256
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
end type

event clicked;If This.Checked Then
	em_Desde.Enabled = False
	em_Hasta.Enabled = False
	em_Desde.Text = ''
	em_Hasta.Text = ''
Else
	em_Desde.Enabled = True
	em_Hasta.Enabled = True
	em_Desde.Text	= String(RelativeDate(Today(), -30), 'dd/mm/yyyy')
	em_Hasta.Text	= String(Today(), 'dd/mm/yyyy')
End If
end event

type uo_selcamara from uo_seleccion_camarasbode within w_info_palletprefrio
event destroy ( )
integer x = 603
integer y = 864
integer taborder = 130
boolean bringtotop = true
end type

on uo_selcamara.destroy
call uo_seleccion_camarasbode::destroy
end on

