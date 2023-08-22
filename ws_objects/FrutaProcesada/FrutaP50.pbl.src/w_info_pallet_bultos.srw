$PBExportHeader$w_info_pallet_bultos.srw
$PBExportComments$Informe de Repalletizajes.
forward
global type w_info_pallet_bultos from w_para_informes
end type
type st_4 from statictext within w_info_pallet_bultos
end type
type st_1 from statictext within w_info_pallet_bultos
end type
type dw_2 from datawindow within w_info_pallet_bultos
end type
type st_6 from statictext within w_info_pallet_bultos
end type
type dw_1 from datawindow within w_info_pallet_bultos
end type
type cbx_agrupa from checkbox within w_info_pallet_bultos
end type
end forward

global type w_info_pallet_bultos from w_para_informes
integer x = 14
integer y = 32
integer width = 2706
integer height = 1288
string title = "INFORME PALLET BULTOS CAJAS NO GENERADAS"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_4 st_4
st_1 st_1
dw_2 dw_2
st_6 st_6
dw_1 dw_1
cbx_agrupa cbx_agrupa
end type
global w_info_pallet_bultos w_info_pallet_bultos

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta,idwc_especie

Integer ii_TipoRepa
end variables

forward prototypes
public function boolean noexistefolioaltura (long al_numero)
public function boolean noexistefolio (long al_numero)
public function boolean existenumero ()
end prototypes

public function boolean noexistefolioaltura (long al_numero);Integer	li_Cliente, li_Planta
Long		ll_existe,ll_Numero, li_cantidad, ll_cantpalcambio, ll_cantparticipacambio, ll_cantinspeccambio

li_Cliente	=	dw_2.Object.clie_codigo[1]
li_Planta	=	dw_1.Object.plde_codigo[1]



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
//ll_Numero	=	Long(em_numero.Text)

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

public function boolean existenumero ();Integer	li_Cliente, li_Planta, li_TipoRepa
Long		ll_Numero

li_Cliente	=	dw_2.Object.clie_codigo[1]
li_Planta	=	dw_1.Object.plde_codigo[1]
ll_Numero	=	Long(Istr_mant.Argumento[2])


SELECT	repe_tipopa
INTO	:ii_TipoRepa
FROM	dbo.repalletenca
WHERE	clie_codigo	=	:li_Cliente
AND	plde_codigo	=	:li_Planta
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

on w_info_pallet_bultos.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.dw_2=create dw_2
this.st_6=create st_6
this.dw_1=create dw_1
this.cbx_agrupa=create cbx_agrupa
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.dw_2
this.Control[iCurrent+4]=this.st_6
this.Control[iCurrent+5]=this.dw_1
this.Control[iCurrent+6]=this.cbx_agrupa
end on

on w_info_pallet_bultos.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.dw_2)
destroy(this.st_6)
destroy(this.dw_1)
destroy(this.cbx_agrupa)
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

type pb_excel from w_para_informes`pb_excel within w_info_pallet_bultos
end type

type st_computador from w_para_informes`st_computador within w_info_pallet_bultos
end type

type st_usuario from w_para_informes`st_usuario within w_info_pallet_bultos
end type

type st_temporada from w_para_informes`st_temporada within w_info_pallet_bultos
end type

type p_logo from w_para_informes`p_logo within w_info_pallet_bultos
end type

type st_titulo from w_para_informes`st_titulo within w_info_pallet_bultos
integer width = 1902
string text = "Informe de Pallet Bultos Cajas Sin Generar"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_pallet_bultos
string tag = "Imprimir Reporte"
integer x = 2327
integer y = 444
integer taborder = 50
end type

event pb_acepta::clicked;SetPointer(Arrow!)
Integer	fila,li_cbx_planta,li_especie,li_numero, li_agrupa
Long		li_planta,li_cliente

istr_info.titulo	= 'INFORME PALLET BULTOS'

OpenWithParm(vinf, istr_info)

IF cbx_agrupa.Checked THEN
	li_agrupa = 1
ELSE
	li_agrupa = 0
END IF	

vinf.dw_1.DataObject = "dw_info_pallet_bultos"

vinf.dw_1.SetTransObject(sqlca)

fila	=	vinf.dw_1.Retrieve(Integer(istr_mant.argumento[3]), &
										Long(istr_mant.argumento[1]),li_agrupa)
	
IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					 StopSign!, Ok!)

ELSE
		F_Membrete(vinf.dw_1)
		
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_pallet_bultos
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2322
integer y = 764
integer taborder = 60
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_info_pallet_bultos
integer x = 247
integer y = 440
integer width = 1902
integer height = 556
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554431
long backcolor = 33554431
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_pallet_bultos
integer x = 347
integer y = 740
integer width = 462
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 8388608
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type dw_2 from datawindow within w_info_pallet_bultos
integer x = 754
integer y = 552
integer width = 1207
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

type st_6 from statictext within w_info_pallet_bultos
integer x = 343
integer y = 560
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 8388608
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_1 from datawindow within w_info_pallet_bultos
integer x = 754
integer y = 728
integer width = 1015
integer height = 92
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]
Integer	li_Cliente

li_Cliente	=	dw_2.Object.clie_codigo[1]

IF ExistePlanta(li_Cliente, Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[1]	=	data
	istr_mant.Argumento[5]	=	ls_Columna[1]
ELSE
	This.SetItem(1, "plde_codigo", gi_CodPlanta)
	
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type cbx_agrupa from checkbox within w_info_pallet_bultos
integer x = 754
integer y = 880
integer width = 713
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 8388608
long backcolor = 553648127
string text = "Agrupa Nº de Caja"
end type

