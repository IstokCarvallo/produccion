$PBExportHeader$w_mant_mues_paramtemporada.srw
$PBExportComments$Ventana de Mantenedor de Temporadas.
forward
global type w_mant_mues_paramtemporada from w_mant_tabla
end type
end forward

global type w_mant_mues_paramtemporada from w_mant_tabla
integer width = 3616
string title = "TEMPORADAS"
event ue_validapassword ( )
end type
global w_mant_mues_paramtemporada w_mant_mues_paramtemporada

type variables
w_mant_deta_paramtemporada iw_mantencion
end variables

forward prototypes
public function integer wf_modifica ()
public function boolean wf_actualiza_db ()
end prototypes

event ue_validapassword();Str_mant		lstr_mant

lstr_mant.Argumento[1]	=	"Producción"
lstr_mant.Argumento[2]	=	gs_Password

OpenWithParm(w_password, lstr_mant)

lstr_mant	=	Message.PowerObjectParm

IF lstr_mant.Respuesta = 0 THEN Close(This)
end event

public function integer wf_modifica ();IF dw_1.AcceptText() = -1 THEN RETURN -1
IF (dw_1.ModifiedCount() + dw_1.DeletedCount()) > 0 THEN RETURN 0
RETURN 1
end function

public function boolean wf_actualiza_db ();Boolean	lb_AutoCommit, lb_Retorno = True	

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF dw_1.Update() = 1 THEN
	Commit;
ELSE
	RollBack;
	
	IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
	
	lb_Retorno	=	False
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

event ue_nuevo();istr_mant.borra	= False
istr_mant.agrega	= True
istr_mant.borra	= False
istr_mant.agrega	= True

OpenWithParm(iw_mantencion, istr_mant)

IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
	pb_eliminar.Enabled	= TRUE
	pb_grabar.Enabled		= TRUE
END IF

dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)

end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "MAESTRO DE TEMPORADAS"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_paramtemporada"

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(0)

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

event ue_recuperadatos();call super::ue_recuperadatos;Long	ll_fila, respuesta,ll_fila1,respuesta1

DO
	ll_fila	= dw_1.Retrieve()
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SelectRow(1,True)
		dw_1.SetFocus()
		pb_imprimir.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
	ELSE
		pb_insertar.SetFocus()
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)

end event

on w_mant_mues_paramtemporada.create
call super::create
end on

on w_mant_mues_paramtemporada.destroy
call super::destroy
end on

event ue_borrar();Long fila

fila	=	dw_1.GetRow()

IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.Borra	=	True
istr_mant.Agrega	=	False

OpenWithParm(iw_mantencion, istr_mant)

istr_mant = Message.PowerObjectParm

IF istr_mant.respuesta = 1 THEN
	IF dw_1.DeleteRow(0) = 1 THEN
	
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF
	
	IF dw_1.RowCount() = 0 THEN
		pb_eliminar.Enabled = False
	ELSE
		il_fila = dw_1.GetRow()
		dw_1.SelectRow(il_fila,True)
		
	END IF
END IF

istr_mant.borra	 = False
end event

event open;x				= 0
y				= 0
This.Width	= dw_1.width + 540
This.Height	= 1993
im_menu	= m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

dw_1.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.dw						=	dw_1
istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()
istr_mant.Solo_Consulta			=	istr_mant.UsuarioSoloConsulta

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)
								

buscar	=	"Código:Npate_tempor,Nombre:Spate_nombre,Abreviacion:Spate_abrevi"
ordenar	=	"Código:pate_tempor,Nombre:pate_nombre,Abreviacion:pate_abrevi"

dw_1.Modify("DataWindow.Footer.Height = 110")


PostEvent("ue_validapassword")
end event

event ue_modifica;call super::ue_modifica;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False
	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_paramtemporada
integer x = 64
integer y = 80
integer width = 3127
integer height = 1388
string dataobject = "dw_mues_paramtemporada"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_paramtemporada
boolean visible = false
integer y = 84
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_paramtemporada
string tag = "Selección de Parámetros"
integer x = 3250
integer y = 136
end type

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_paramtemporada
string tag = ""
boolean visible = false
integer x = 3250
integer y = 444
end type

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_paramtemporada
string tag = ""
integer x = 3250
integer y = 580
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_paramtemporada
string tag = ""
integer x = 3250
integer y = 808
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_paramtemporada
string tag = ""
integer x = 3250
integer y = 1024
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_paramtemporada
string tag = ""
integer x = 3250
integer y = 1240
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_paramtemporada
string tag = ""
integer x = 3250
integer y = 1456
end type

