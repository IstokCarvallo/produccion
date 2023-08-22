$PBExportHeader$w_info_existenciaonline.srw
$PBExportComments$Informe Consolidado General de Embarques.
forward
global type w_info_existenciaonline from w_para_informes
end type
type st_1 from statictext within w_info_existenciaonline
end type
type dw_1 from uo_dw within w_info_existenciaonline
end type
type st_mensaje from statictext within w_info_existenciaonline
end type
end forward

global type w_info_existenciaonline from w_para_informes
integer width = 2597
integer height = 1180
event ue_listo ( )
st_1 st_1
dw_1 dw_1
st_mensaje st_mensaje
end type
global w_info_existenciaonline w_info_existenciaonline

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

on w_info_existenciaonline.create
int iCurrent
call super::create
this.st_1=create st_1
this.dw_1=create dw_1
this.st_mensaje=create st_mensaje
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.dw_1
this.Control[iCurrent+3]=this.st_mensaje
end on

on w_info_existenciaonline.destroy
call super::destroy
destroy(this.st_1)
destroy(this.dw_1)
destroy(this.st_mensaje)
end on

event open;call super::open;dw_1.SetTransObject(Sqlca)

st_Mensaje.Text = 'Preparando Generacion de Archivo...'

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)
end event

event close;call super::close;GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)
end event

type pb_excel from w_para_informes`pb_excel within w_info_existenciaonline
boolean visible = true
integer x = 2176
integer y = 292
fontcharset fontcharset = ansi!
string facename = "Tahoma"
boolean enabled = true
boolean default = true
end type

event pb_excel::clicked;call super::clicked;SetPointer(HourGlass!)

Long		ll_fila, ll_cierre
String 	ls_path, ls_file

dw_1.SetTransObject(sqlca)

st_Mensaje.Text = 'Recuperando Informacion...'
ll_fila = dw_1.Retrieve()

IF ll_fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	If GetFileSaveName( "Seleccione archivo",  ls_path, ls_file, "Excel", ".XLS Files (*.xls),*.xls" , "C:\") = -1 Then
		MessageBox('Error', 'No se encontro archivo solicitdo.' , StopSign!, OK! )
		Return -1
	End If

	If dw_1.SaveAs(ls_File, Excel8!, True) = -1 Then
		MessageBox('Error', 'No se pùdo generar archivo ('+ ls_file +') con informción solicitda.' , StopSign!, OK! )
		Return -1
	Else
		MessageBox('Atencion', 'Archivo ('+ ls_file +') generado satisfactoriamente.' , Information!, OK! )
		st_Mensaje.Text = 'Proceso Terminado...'
	End If
END IF

SetPointer(Arrow!)

end event

type st_computador from w_para_informes`st_computador within w_info_existenciaonline
integer x = 1673
integer textsize = -10
fontfamily fontfamily = swiss!
string facename = "Tahoma"
end type

type st_usuario from w_para_informes`st_usuario within w_info_existenciaonline
integer x = 1673
integer textsize = -10
fontfamily fontfamily = swiss!
string facename = "Tahoma"
end type

type st_temporada from w_para_informes`st_temporada within w_info_existenciaonline
integer x = 1673
integer textsize = -10
fontfamily fontfamily = swiss!
string facename = "Tahoma"
end type

type p_logo from w_para_informes`p_logo within w_info_existenciaonline
end type

type st_titulo from w_para_informes`st_titulo within w_info_existenciaonline
integer x = 247
integer width = 1897
string text = "Generacion Existencia OnLine"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_existenciaonline
string tag = "Imprimir Reporte"
boolean visible = false
integer x = 2190
integer y = 512
integer taborder = 110
fontcharset fontcharset = ansi!
string facename = "Tahoma"
boolean enabled = false
boolean default = false
end type

type pb_salir from w_para_informes`pb_salir within w_info_existenciaonline
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2190
integer y = 800
integer taborder = 120
fontcharset fontcharset = ansi!
string facename = "Tahoma"
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_1 from statictext within w_info_existenciaonline
integer x = 247
integer y = 408
integer width = 1897
integer height = 552
integer textsize = -10
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

type dw_1 from uo_dw within w_info_existenciaonline
boolean visible = false
integer x = 1307
integer y = 84
integer width = 229
integer height = 164
integer taborder = 10
boolean bringtotop = true
string dataobject = "dw_gene_existenciaonline"
boolean vscrollbar = false
end type

type st_mensaje from statictext within w_info_existenciaonline
integer x = 302
integer y = 480
integer width = 1783
integer height = 360
boolean bringtotop = true
integer textsize = -12
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = script!
string facename = "Comic Sans MS"
long textcolor = 65535
long backcolor = 553648127
borderstyle borderstyle = StyleLowered!
boolean focusrectangle = false
end type

