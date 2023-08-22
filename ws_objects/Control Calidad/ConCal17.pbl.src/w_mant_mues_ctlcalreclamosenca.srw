$PBExportHeader$w_mant_mues_ctlcalreclamosenca.srw
$PBExportComments$Mantenedor para reclamos con datos incompletos (Inf. Zonal, Fecha Inf. etc..)
forward
global type w_mant_mues_ctlcalreclamosenca from w_mant_directo
end type
type uo_selespe from uo_seleccion_especie_mod within w_mant_mues_ctlcalreclamosenca
end type
end forward

global type w_mant_mues_ctlcalreclamosenca from w_mant_directo
integer width = 3808
string title = "Reclamos Pendientes"
uo_selespe uo_selespe
end type
global w_mant_mues_ctlcalreclamosenca w_mant_mues_ctlcalreclamosenca

type variables
Integer ii_especie
end variables

event ue_recuperadatos;call super::ue_recuperadatos;dw_1.Retrieve(ii_especie)
end event

on w_mant_mues_ctlcalreclamosenca.create
int iCurrent
call super::create
this.uo_selespe=create uo_selespe
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.uo_selespe
end on

on w_mant_mues_ctlcalreclamosenca.destroy
call super::destroy
destroy(this.uo_selespe)
end on

event open;call super::open;boolean lb_cerrar

x				=	0
y				=	0
This.Width	=	dw_1.width +392
This.Height	=	2200	

ii_especie =  Integer(Message.StringParm)

If IsNull(uo_selespe.Codigo)		Then lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else
	uo_selespe.Seleccion(False, False)

	// Inicia en UVAS
	uo_selespe.dw_seleccion.Object.codigo[1] = ii_especie
	uo_selespe.iuo_Especie.Existe(ii_especie, False, Sqlca)
	uo_selespe.Codigo 	= ii_especie
	uo_selespe.Nombre	= uo_selespe.iuo_Especie.Nombre
	uo_selespe.Enabled = False
	uo_selespe.dw_Seleccion.Object.codigo.BackGround.Color	=	553648127
	
	PostEvent("ue_recuperadatos")
end if
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_ctlcalreclamosenca
integer x = 87
integer y = 80
integer width = 2080
boolean enabled = true
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_ctlcalreclamosenca
integer x = 3511
end type

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_ctlcalreclamosenca
integer x = 3515
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_ctlcalreclamosenca
integer x = 3511
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_ctlcalreclamosenca
integer x = 3506
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_ctlcalreclamosenca
integer x = 3511
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_ctlcalreclamosenca
boolean visible = false
integer x = 3520
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_ctlcalreclamosenca
integer x = 3511
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_ctlcalreclamosenca
integer y = 260
integer width = 2089
string dataobject = "dw_mant_ctlcalreclamosencapend"
end type

type uo_selespe from uo_seleccion_especie_mod within w_mant_mues_ctlcalreclamosenca
integer x = 635
integer y = 108
integer taborder = 20
boolean bringtotop = true
end type

on uo_selespe.destroy
call uo_seleccion_especie_mod::destroy
end on

