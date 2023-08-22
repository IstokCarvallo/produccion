$PBExportHeader$w_maed_provincias_ciudades.srw
forward
global type w_maed_provincias_ciudades from w_mant_encab_deta
end type
end forward

global type w_maed_provincias_ciudades from w_mant_encab_deta
integer width = 2711
string title = "Povincias / Ciudades"
string menuname = ""
event ue_imprimir ( )
end type
global w_maed_provincias_ciudades w_maed_provincias_ciudades

type variables
w_mant_deta_ciudades    iw_mantencion
end variables

forward prototypes
public subroutine habilitaencab (boolean habilita)
public subroutine habilitabotton (string columna)
public subroutine existeprovincia (integer codigo)
end prototypes

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "MAESTRO DE PROVINVIAS /  CIUDADES"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)
vinf.dw_1.DataObject = "dw_info_provinciaciudades"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(Integer(istr_mant.argumento[1]), Integer(istr_mant.argumento[2]))

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de Datos : ~n" + sqlca.SQLErrText, StopSign!, OK!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No Existe información para este informe.", StopSign!, OK!)	
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
					
end event

public subroutine habilitaencab (boolean habilita);IF Habilita Then
	dw_2.Object.regi_codigo.Protect				=	0
	dw_2.Object.prov_codigo.Protect				=	0
	dw_2.Object.regi_codigo.Color					= 0
	dw_2.Object.prov_codigo.Color					= 0
	dw_2.Object.regi_codigo.BackGround.Color	= Rgb(255,255,255)
	dw_2.Object.prov_codigo.BackGround.Color	= Rgb(255,255,255)
Else
	dw_2.Object.regi_codigo.Protect				= 1
	dw_2.Object.prov_codigo.Protect				= 1
	dw_2.Object.regi_codigo.Color					= Rgb(255,255,255)
	dw_2.Object.prov_codigo.Color					= Rgb(255,255,255)
	dw_2.Object.regi_codigo.BackGround.Color	= 553648127
	dw_2.Object.prov_codigo.BackGround.Color	= 553648127
End If


end subroutine

public subroutine habilitabotton (string columna);IF IsNull(dw_2.GetItemNumber(il_fila, "prov_codigo")) = True OR &
	dw_2.GetItemNumber(1, "prov_codigo") > 0 THEN
	pb_ins_det.Enabled = True
END IF

IF IsNull(dw_2.GetItemNumber(il_fila, "regi_codigo")) = True OR &
	dw_2.GetItemNumber(1, "regi_codigo") > 0 THEN
	pb_ins_det.Enabled = True
END IF

end subroutine

public subroutine existeprovincia (integer codigo);String 	ls_nombre

istr_mant.argumento[2] = String(codigo)

SELECT	prov_nombre INTO :ls_nombre
	FROM	dbo.provincias
	WHERE	regi_codigo = :istr_mant.argumento[1]
	AND   prov_codigo = :codigo;

IF SQLCA.SqlCode = -1 THEN
	MessageBox("Error","Error al intentar conección a Base de Datos",Information!, OK!)
	dw_1.SetColumn("prov_codigo")
ELSEIF SQLCA.SqlCode = 0 THEN
	istr_mant.argumento[3] = ls_nombre
	This.TriggerEvent("ue_recuperadatos")
END IF

RETURN
end subroutine

on w_maed_provincias_ciudades.create
call super::create
end on

on w_maed_provincias_ciudades.destroy
call super::destroy
end on

event open;call super::open;buscar  = "Código:Nregi_codigo,Código Provincia:Nprov_codigo"
ordenar = "Código:regi_codigo,Código Provincia:prov_codigo"
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila_d, ll_fila_e, respuesta

DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	ll_fila_e	= dw_2.Retrieve(Integer(istr_mant.argumento[1]),&
										 Integer(istr_mant.argumento[2]))
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		HabilitaEncab(False)
		
		DO
			ll_fila_d	= dw_1.Retrieve(Integer(istr_mant.argumento[1]),&
												 Integer(istr_mant.argumento[2]))
			
			IF ll_fila_d = -1 THEN
				respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				pb_eliminar.Enabled  = True
				pb_grabar.Enabled		= True
				pb_ins_det.Enabled	= True
				pb_imprimir.Enabled	= True
				
				IF ll_fila_d > 0 THEN
					pb_eli_det.Enabled	= True
					dw_1.SetRow(1)
					dw_1.SelectRow(1,True)
					dw_1.SetFocus()
				ELSE
					pb_ins_det.SetFocus()
				END IF
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)


		
		
end event

event ue_borra_detalle;call super::ue_borra_detalle;Long	ll_fila

IF dw_1.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicrohelp("Validando la eliminacion de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra	= True
istr_mant.agrega	= False

OpenWithParm(iw_mantencion, istr_mant)

istr_mant = Message.PowerObjectParm

IF istr_mant.respuesta = 1 THEN
	
	IF dw_1.DeleteRow(0) = 1 THEN
		ib_borrar = False
		W_main.SetMicroHelp("Borrando Registro...")
		ll_fila = dw_1.GetRow()
		dw_1.SetRow(ll_fila)
		dw_1.SelectRow(ll_fila, True)
		SetPointer(arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF
	IF dw_1.RowCount() = 0 THEN
		habilitaencab(True)
		pb_eli_det.Enabled = False
	END IF
END IF

istr_mant.borra	= False
end event

event ue_seleccion;call super::ue_seleccion;istr_busq.argum[2] =  "" 

IF IsNull(dw_2.Object.regi_codigo[1]) THEN
	istr_busq.argum[1] = '0'
ELSE
	istr_busq.argum[1] = String(dw_2.Object.regi_codigo[1])
END IF
OpenWithParm(w_busc_provincias,istr_busq)

istr_busq	= Message.PowerObjectParm

IF istr_busq.argum[2] <> "" THEN
	istr_mant.argumento[1] = istr_busq.argum[1]
	istr_mant.argumento[2] = istr_busq.argum[2]
	istr_mant.argumento[3] = istr_busq.argum[3]
	This.TriggerEvent("ue_recuperadatos")
ELSE
	pb_buscar.SetFocus()
END IF
end event

event ue_nuevo_detalle;call super::ue_nuevo_detalle;Long fial

istr_mant.borra	= False
istr_mant.agrega	= True

OpenWithParm(iw_mantencion, istr_mant)

IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = False THEN
	pb_eliminar.Enabled	= True
	pb_grabar.Enabled		= True
END IF

dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)
end event

event ue_nuevo;call super::ue_nuevo;HabilitaEncab(True)
dw_2.SetColumn("regi_codigo")
end event

event ue_modifica_detalle;call super::ue_modifica_detalle;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False

	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

type dw_1 from w_mant_encab_deta`dw_1 within w_maed_provincias_ciudades
integer x = 59
integer y = 940
integer width = 2066
integer height = 800
integer taborder = 100
string title = "Ciudades"
string dataobject = "dw_mues_prov_ciudades"
end type

type dw_2 from w_mant_encab_deta`dw_2 within w_maed_provincias_ciudades
integer x = 78
integer y = 72
integer width = 1737
integer height = 768
integer taborder = 10
string dataobject = "dw_mant_provincias"
end type

event dw_2::clicked;call super::clicked;//
end event

event dw_2::doubleclicked;call super::doubleclicked;//
end event

event dw_2::itemchanged;String	ls_campo

ls_campo = dwo.name

CHOOSE CASE ls_campo
	CASE "regi_codigo"
		istr_mant.argumento[1] = data
		
	CASE "prov_codigo"
		Existeprovincia(Integer(data))
	
	CASE "prov_nombre"
		istr_mant.argumento[3] = data
		HabilitaBotton(dwo.name)

END CHOOSE

HabilitaBotton(dwo.name)
end event

type pb_nuevo from w_mant_encab_deta`pb_nuevo within w_maed_provincias_ciudades
integer x = 2299
integer y = 284
integer taborder = 30
end type

type pb_eliminar from w_mant_encab_deta`pb_eliminar within w_maed_provincias_ciudades
integer x = 2299
integer y = 464
integer taborder = 40
end type

type pb_grabar from w_mant_encab_deta`pb_grabar within w_maed_provincias_ciudades
integer x = 2299
integer y = 640
integer taborder = 50
end type

type pb_imprimir from w_mant_encab_deta`pb_imprimir within w_maed_provincias_ciudades
integer x = 2299
integer y = 820
integer taborder = 60
end type

type pb_salir from w_mant_encab_deta`pb_salir within w_maed_provincias_ciudades
integer x = 2299
integer y = 1004
integer taborder = 90
end type

type pb_ins_det from w_mant_encab_deta`pb_ins_det within w_maed_provincias_ciudades
integer x = 2313
integer y = 1296
integer taborder = 70
end type

type pb_eli_det from w_mant_encab_deta`pb_eli_det within w_maed_provincias_ciudades
integer x = 2313
integer y = 1472
integer taborder = 80
end type

type pb_buscar from w_mant_encab_deta`pb_buscar within w_maed_provincias_ciudades
integer x = 2304
integer y = 100
integer taborder = 20
end type

