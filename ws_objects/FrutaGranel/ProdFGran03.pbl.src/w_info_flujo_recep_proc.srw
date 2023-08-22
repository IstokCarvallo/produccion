$PBExportHeader$w_info_flujo_recep_proc.srw
$PBExportComments$Ventana de Informe de numerales
forward
global type w_info_flujo_recep_proc from w_para_informes
end type
type dw_1 from datawindow within w_info_flujo_recep_proc
end type
end forward

global type w_info_flujo_recep_proc from w_para_informes
integer x = 14
integer y = 32
integer width = 2802
integer height = 988
string title = "Flujo de Recepción y Proceso"
string icon = "\Desarrollo\Produccion_3-2\ProdFrutaGranel\ProdFGranel.ico"
dw_1 dw_1
end type
global w_info_flujo_recep_proc w_info_flujo_recep_proc

type variables

uo_plantadesp			iuo_plantadesp
uo_especie				iuo_especie
uo_productores			iuo_productores

DataWindowChild	idwc_planta, idwc_especie, idwc_productor

String 				is_planta, is_especie, is_productor
end variables

on w_info_flujo_recep_proc.create
int iCurrent
call super::create
this.dw_1=create dw_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_1
end on

on w_info_flujo_recep_proc.destroy
call super::destroy
destroy(this.dw_1)
end on

event open;x	=	0
y	=	0

iuo_plantadesp			=	Create uo_plantadesp
iuo_especie				=	Create uo_especie
iuo_productores		=	Create uo_productores

//Planta
dw_1.GetChild("planta", idwc_planta)
idwc_planta.SetTransObject(sqlca)
IF idwc_planta.Retrieve(gi_codexport) = 0 THEN
	MessageBox("Atención","Falta Registrar Plantas")
	idwc_planta.InsertRow(0)
END IF

//Especie
dw_1.GetChild("especie", idwc_especie)
idwc_especie.SetTransObject(sqlca)
IF idwc_especie.Retrieve(gstr_parempresa.empr_codexp) = 0 THEN
	MessageBox("Atención","Falta Registrar Especies")
	idwc_especie.InsertRow(0)
END IF

//Productor
dw_1.GetChild("productor",idwc_productor)
idwc_productor.SetTransObject(Sqlca)
IF idwc_productor.Retrieve(gi_codexport) = 0 THEN
	MessageBox("Atención","Falta Registrar Productores")
	idwc_productor.InsertRow(0)
END IF

dw_1.SetTransObject(SQLCA)
dw_1.InsertRow(0)

Paramtemporada(gstr_paramtempo)
dw_1.SetItem(1,"fechad", gstr_paramtempo.fechainicio)
dw_1.SetItem(1,"fechah", Today())
dw_1.setitem(1,"planta", gstr_paramplanta.codigoplanta)
dw_1.setitem(1,"especie", gstr_paramplanta.codigoespecie)

IF iuo_PlantaDesp.Existe(gstr_paramplanta.codigoplanta,True,Sqlca) THEN
	is_planta = iuo_PlantaDesp.nombre
END IF

IF iuo_especie.Existe(gstr_paramplanta.codigoespecie,True,Sqlca) THEN
	is_especie = iuo_especie.nombre
END IF
end event

type st_titulo from w_para_informes`st_titulo within w_info_flujo_recep_proc
integer x = 105
integer y = 44
integer width = 2240
string text = "Informe Flujo de Recepción y Proceso"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_flujo_recep_proc
integer x = 2473
integer y = 260
integer height = 140
integer taborder = 370
end type

event pb_acepta::clicked;SetPointer(Arrow!)
 
Integer	li_Planta, li_Especie, li_Productor
Long		ll_Fila
Date		ld_Fechadesde, ld_FechaHasta, ld_fechaprueba
String	ls_fecha 

istr_info.titulo	= ''

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_flujo_recep_proc"

dw_1.accepttext()

// Acepta Planta //
li_Planta = dw_1.Object.planta[1]
IF IsNull(li_Planta) Then
	MessageBox( "Planta Errónea", "Falta seleccionar una Planta.", &
				 StopSign!, Ok!)
	RETURN				 
END IF

li_Especie = dw_1.Object.especie[1]
IF IsNull(li_especie) Then
	MessageBox( "Especie Errónea", "Falta seleccionar una Especie.", &
                StopSign!, Ok!)
	RETURN
END IF

// Acepta Productor
IF dw_1.Object.todosprod[1] = 1 THEN
	li_Productor = 0
	is_productor = 'Todos'	
ELSE
	li_Productor = dw_1.Object.productor[1]
	If IsNull(li_Productor) Then
		MessageBox( "Productor Erróneo", "Falta seleccionar un Productor.", &
	             StopSign!, Ok!)
		RETURN				 
   END If
END IF

ld_fechadesde = dw_1.Object.fechad[1]

If IsNull(ld_fechadesde) or ld_fechadesde<= ld_fechaprueba Then
	MessageBox( "Fecha Erronea", "Se debe Ingresar una Fecha Desde.", &
	             StopSign!, Ok!)
	RETURN			 
END If

ls_fecha			=	mid(String(dw_1.Object.fechah[1],'dd/mm/yyyy'),1,10)
ld_fechahasta 	= 	Date(ls_fecha)

If IsNull(ld_fechahasta) or ld_fechahasta<=ld_fechaprueba Then
	MessageBox( "Fecha Erronea", "Se debe Ingresar una Fecha Hasta.", &
	             StopSign!, Ok!)
	RETURN				 
END If

IF ld_fechadesde > ld_fechahasta THEN
	MessageBox( "Fechas Erroneas", "La Fecha Hasta debe ser mayor o igual a la Fecha Desde.", &
	             StopSign!, Ok!)
	RETURN
END IF

vinf.dw_1.SetTransObject(sqlca)

ll_Fila	=	vinf.dw_1.Retrieve(li_Planta, li_especie, li_productor,ld_fechadesde, ld_fechahasta)

IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)

ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 100')

	vinf.dw_1.Object.planta.text 		=  is_planta
	vinf.dw_1.Object.especie.text 	=  is_especie
	vinf.dw_1.Object.productor.text 	=  is_productor		
	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_flujo_recep_proc
integer x = 2473
integer y = 548
integer height = 140
integer taborder = 380
end type

type dw_1 from datawindow within w_info_flujo_recep_proc
integer x = 105
integer y = 156
integer width = 2240
integer height = 648
integer taborder = 60
string title = "none"
string dataobject = "dw_info_flujo_recep_sel"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;call super::itemchanged;Integer	li_Null,li_cliente
String	ls_Columna

SetNull(li_Null)

dw_1.accepttext()
li_cliente  =  dw_1.Object.exportador[1]

ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna
		
	CASE "planta"
		IF NOT iuo_PlantaDesp.Existe(Integer(data),True,Sqlca) THEN
			This.SetItem(1, "planta", li_Null )
			This.SetFocus()
			RETURN 1
		ELSE
			is_planta = iuo_PlantaDesp.nombre
		END IF		
		
		
	CASE "especie"

		IF NOT iuo_Especie.Existe(Integer(data),True,SqlCa) THEN
			This.SetItem(1, "especie", li_Null )
			This.SetFocus()
			RETURN 1
		ELSE
			is_especie = iuo_especie.nombre
		END IF
	

	CASE "productor"
		IF NOT iuo_Productores.Existe(Long(data),True,SqlCa) THEN
			This.SetItem(1, "productor", li_Null)
			This.SetFocus()
			RETURN 1
		ELSE
			is_productor = iuo_Productores.nombre
		END IF
		
	CASE	"todosprod"
		IF	data = '1' THEN 
			This.Object.productor[row]		=	li_Null
		END IF
		
END CHOOSE
end event

event itemerror;RETURN 1
end event

