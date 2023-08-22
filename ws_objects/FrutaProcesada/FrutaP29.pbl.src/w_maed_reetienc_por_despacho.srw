$PBExportHeader$w_maed_reetienc_por_despacho.srw
$PBExportComments$Ventana Mantenedor Encabezado de Reetiquetado.
forward
global type w_maed_reetienc_por_despacho from w_mant_encab_deta_csd
end type
type dw_3 from datawindow within w_maed_reetienc_por_despacho
end type
end forward

global type w_maed_reetienc_por_despacho from w_mant_encab_deta_csd
integer width = 3099
integer height = 2056
string title = "MANTENCION DE REETIQUETADO"
string menuname = ""
boolean maxbox = false
event ue_imprimir ( )
dw_3 dw_3
end type
global w_maed_reetienc_por_despacho w_maed_reetienc_por_despacho

type variables
w_mant_deta_reetidet iw_mantencion

DataWindowChild	dw_planta, dw_plades, dw_puerto,dw_etiquetas,dw_plantas,dw_clientes,&
						idwc_etiquetaant,dw_etiqueta,idwc_etiqueta
end variables

forward prototypes
public function boolean noexistecliente (integer ai_codigo)
public subroutine habilitaingreso (string columna)
public function boolean existereetiquetado (string as_columna, string as_valor)
public subroutine habilitaencab (boolean habilita)
public function boolean existeetiqueta (string as_columna, string as_valor)
public function boolean existeguia (string as_columna, string as_valor)
public subroutine reemplazaetiqueta (integer li_etiqueta)
protected function boolean wf_actualiza_db (boolean borrando)
public function long buscareetiqueta (integer planta)
end prototypes

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "REETIQUETADO DE PALLETS"
lstr_info.copias	= 1



OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_reetiquetados"

vinf.dw_1.SetTransObject(sqlca)

vinf.dw_1.GetChild("etiq_codigo", idwc_etiqueta)
idwc_etiqueta.SetTransObject(sqlca)
idwc_etiqueta.Retrieve(Integer(istr_mant.argumento[3]))

vinf.dw_1.GetChild("etiq_numant", idwc_etiquetaant)
idwc_etiquetaant.SetTransObject(sqlca)
idwc_etiquetaant.Retrieve(Integer(istr_mant.argumento[3]))

fila = vinf.dw_1.Retrieve(Long(istr_mant.argumento[2]), &
									Integer(istr_mant.argumento[3]), &
									Integer(istr_mant.argumento[1]))

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

public function boolean noexistecliente (integer ai_codigo);String	ls_nombre

SELECT	clie_nombre
	INTO	:ls_nombre  
   FROM	dbo.clientesprod  
   WHERE	clie_codigo =	:ai_codigo;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Clientes Producción")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Error", "Cliente no Existe. Ingrese otro.")
	RETURN True
END IF

IF F_ValidaCliente(ai_codigo) THEN
	istr_mant.Argumento[3]	=	String(ai_codigo)
	RETURN False
ELSE
	RETURN True
END IF
end function

public subroutine habilitaingreso (string columna);Date		ld_fecha
Boolean	lb_estado = True

dw_2.AcceptText()

IF dw_2.RowCount() > 0 THEN
	IF IsNull(dw_2.Object.plde_codigo[1]) OR dw_2.Object.plde_codigo[1] = 0 OR &
		IsNull(dw_2.Object.reet_fereet[1]) OR dw_2.Object.reet_fereet[1] = ld_fecha OR &
		IsNull(dw_2.Object.defe_numero[1]) OR dw_2.Object.defe_numero[1] = 0 OR &
		IsNull(dw_2.Object.etiq_codigo[1]) OR dw_2.Object.etiq_codigo[1] = 0  THEN
		lb_estado = False
	END IF
END IF

pb_grabar.Enabled = lb_estado
end subroutine

public function boolean existereetiquetado (string as_columna, string as_valor);Integer	li_Planta, li_Existe, li_Cliente
Long		ll_Numero

ll_Numero 	=	dw_2.Object.reet_numero[1]
li_Cliente	=	dw_2.Object.clie_codigo[1]
li_Planta	=	dw_2.Object.plde_codigo[1]

CHOOSE CASE as_columna
	CASE "clie_codigo"
		li_Cliente	=	Integer(as_valor)
		
	CASE "plde_codigo"
		li_Planta	=	Integer(as_valor)

	CASE "reet_numero"
		ll_Numero 	=	Long(as_valor)
		
END CHOOSE

SELECT	Count(*) 
	INTO	 :li_Existe
	FROM	dbo.REETIENC
	WHERE	reet_numero	=	:ll_Numero
	AND	clie_codigo =  :li_Cliente
	AND	plde_codigo	=	:li_Planta;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Reetienc")
	RETURN True
ELSE
	IF li_existe > 0 THEN

//		istr_mant.argumento[2]	=	String(ll_Numero)
//		istr_mant.argumento[1]	=	String(li_Planta)
//		This.TriggerEvent("ue_recuperadatos")
//		istr_mant.argumento[3]	=	String(dw_2.Object.clie_codigo[1])
//		istr_mant.argumento[8]	=	''
      Messagebox('Existe','Este Correlativo No Puede Ser Visto En Esta Ventana')		
		RETURN True
	END IF
   RETURN False
END IF
end function

public subroutine habilitaencab (boolean habilita);If Habilita Then
	dw_2.Object.clie_codigo.Protect	=	0
	dw_2.Object.plde_codigo.Protect	=	0
	dw_2.Object.reet_fereet.Protect	=	0
	dw_2.Object.defe_numero.Protect	=	0
	dw_2.Object.etiq_codigo.Protect	=	0
	dw_2.Object.reet_observ.Protect	=	0
	
	dw_2.Object.clie_codigo.Color 		= 0
	dw_2.Object.plde_codigo.Color 	= 0
	dw_2.Object.reet_fereet.Color 		= 0
	dw_2.Object.defe_numero.Color 	= 0
	dw_2.Object.etiq_codigo.Color		= 0
	dw_2.Object.reet_observ.Color		= 0
	
	dw_2.Object.clie_codigo.BackGround.Color 		= RGB(255,255,255)
	dw_2.Object.plde_codigo.BackGround.Color 	= RGB(255,255,255)
	dw_2.Object.reet_fereet.BackGround.Color 		= RGB(255,255,255)
	dw_2.Object.defe_numero.BackGround.Color 	= RGB(255,255,255)
	dw_2.Object.etiq_codigo.BackGround.Color		= RGB(255,255,255)
	dw_2.Object.reet_observ.BackGround.Color		= RGB(255,255,255)

	dw_2.SetColumn("reet_numero")
	dw_2.SetFocus()
Else
	dw_2.Object.clie_codigo.Protect	=	1
	dw_2.Object.plde_codigo.Protect	=	1
	dw_2.Object.reet_fereet.Protect	=	1
	dw_2.Object.defe_numero.Protect	=	1
	dw_2.Object.etiq_codigo.Protect	=	1
	dw_2.Object.reet_observ.Protect	=	1
	
	dw_2.Object.clie_codigo.Color 		= RGB(255,255,255)
	dw_2.Object.plde_codigo.Color 	= RGB(255,255,255)
	dw_2.Object.reet_fereet.Color 		= RGB(255,255,255)
	dw_2.Object.defe_numero.Color 	= RGB(255,255,255)
	dw_2.Object.etiq_codigo.Color		= RGB(255,255,255)
	dw_2.Object.reet_observ.Color		= RGB(255,255,255)
	
	dw_2.Object.clie_codigo.BackGround.Color 		= 553648127
	dw_2.Object.plde_codigo.BackGround.Color 	= 553648127
	dw_2.Object.reet_fereet.BackGround.Color 		= 553648127
	dw_2.Object.defe_numero.BackGround.Color 	= 553648127
	dw_2.Object.etiq_codigo.BackGround.Color		= 553648127
	dw_2.Object.reet_observ.BackGround.Color		= 553648127
End If

pb_grabar.Enabled	=	False
end subroutine

public function boolean existeetiqueta (string as_columna, string as_valor);Integer	li_cliente, li_etiqueta
String	ls_nombre

li_cliente	=	dw_2.Object.clie_codigo[1]
li_etiqueta	=	dw_2.Object.etiq_codigo[1]

CHOOSE CASE as_columna
	CASE "etiq_codigo"
		li_etiqueta	=	Integer(as_valor)
		
END CHOOSE

SELECT	etiq_nombre
	INTO	 :ls_nombre
	FROM	dbo.etiquetas
	WHERE	etiq_codigo	=	:li_etiqueta;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Etiquetas")
	RETURN True
ELSE	
	istr_mant.argumento[10]	=	String(li_etiqueta)
   RETURN False
END IF
end function

public function boolean existeguia (string as_columna, string as_valor);Integer	li_Planta, li_Existe, li_Cliente
Long		ll_Numero

ll_Numero 	=	dw_2.Object.defe_numero[1]
li_Cliente	=	dw_2.Object.clie_codigo[1]
li_Planta	=	dw_2.Object.plde_codigo[1]

CHOOSE CASE as_columna
	CASE "clie_codigo"
		li_Cliente	=	Integer(as_valor)
		
	CASE "plde_codigo"
		li_Planta	=	Integer(as_valor)

	CASE "defe_numero"
		ll_Numero 	=	Long(as_valor)
		
END CHOOSE

SELECT	Count(*) 
	INTO	 :li_Existe
	FROM	dbo.despafrigoen
	WHERE	defe_guides	=	:ll_Numero
	AND	clie_codigo =  :li_Cliente
	AND	plde_codigo	=	:li_Planta;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Despafrigoen")
	RETURN True
ELSE
	IF li_existe > 0 THEN
		istr_mant.argumento[1]	=	String(li_Planta)
		istr_mant.argumento[2]	=	String(ll_Numero)
		istr_mant.argumento[3]	=	String(li_cliente)
		This.TriggerEvent("ue_recuperadatos")
		RETURN False
	ELSE
		//MessageBox("NO EXISTE!","Guía del Cliente y Planta Seleccionado, No Existe !!!")
		dw_1.Reset()
		dw_3.Reset()
		
	END IF
   RETURN False
END IF
end function

public subroutine reemplazaetiqueta (integer li_etiqueta);Integer	li_secuen, li_etiqant

IF dw_3.Rowcount()>0 THEN
	
	FOR li_secuen=1 TO dw_1.RowCount()
	
		li_etiqant	=	dw_1.Object.etiq_codigo[li_secuen]
		
		dw_1.SetItem(li_secuen, "etiq_numant", li_etiqant)
		dw_1.SetItem(li_secuen, "etiq_codigo", li_etiqueta)
	
	NEXT
	
END IF

end subroutine

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno
Long		ll_numero, Numero
Integer	li_etiqueta, li_Planta, li_movto

ll_numero	=	Long(istr_mant.argumento[2])
li_etiqueta	=	Integer(istr_mant.argumento[10])

IF Not dw_2.uf_check_required(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	IF dw_3.Update(True, False) = 1 THEN
		IF dw_2.Update(True, False) = 1 THEN
			Commit;
			
			IF sqlca.SQLCode <> 0 THEN
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			ELSE
				lb_Retorno	=	True
				
				dw_3.ResetUpdate()
				dw_2.ResetUpdate()
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
	END IF
ELSE
	IF dw_2.Update(True, False) = 1 THEN
		IF dw_3.Update(True, False) = 1 THEN
			Commit;
			
			IF sqlca.SQLCode <> 0 THEN
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			ELSE
				lb_Retorno	=	True
				
				dw_3.ResetUpdate()
				dw_2.ResetUpdate()
				
				UPDATE dba.palletencab as pae, dba.despafrigoen as dee, dba.despafrigode as ded SET
				pae.etiq_codigo=li_etiqueta
				WHERE ded.plde_codigo=dee.plde_codigo
				AND   ded.clie_codigo=dee.clie_codigo
				AND   ded.defe_numero=dee.defe_numero
				AND   ded.clie_codigo=pae.clie_codigo
				AND   ded.plde_codigo=pae.plde_codigo
				AND   ded.paen_numero=pae.paen_numero
				AND   dee.defe_guides=:ll_numero;
				
				Numero = Long(istr_mant.argumento[15])
				li_planta = Integer(istr_mant.argumento[1])
				
				li_movto = 8
				
				/*actualiza numero actual en correlativos */
				update DBA.CORRELMOVIMIENTOS set
				COMO_ACTUAl = :numero
				where plde_codigo = :li_planta
				And	como_tipomv = :li_movto;				
				
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
	END IF
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno

end function

public function long buscareetiqueta (integer planta);Integer	li_planta, li_tipoins, li_movto
Long		ll_numero,ll_numero2, ll_fin, ll_actual

li_planta	=	planta

li_movto = 8

Select max(reet_numero) 
Into  :ll_numero
From dbo.reetienc
Where plde_codigo = :li_planta;

Select como_inicia, como_actual, como_termin
Into	:ll_numero2, :ll_actual, :ll_fin
from dbo.CORRELMOVIMIENTOS 
Where plde_codigo = :li_planta
and	COMO_TIPOMV = :li_movto;

IF ll_actual >= ll_fin THEN
	Return 0
END IF	

ll_fin = ll_fin - 3

IF ll_actual >= ll_fin THEN 
	MessageBox("Advertencia","Quedan menos de 3 Correlativos, Proceda por Mantención 'Correlativos'")
END IF	

IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla CORRELMOVIMIENTOS")
END IF

IF Isnull(ll_numero) OR String(ll_numero) = '' or ll_numero < ll_numero2 THEN
	ll_numero = ll_numero2
END IF	
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Reetienc")
ELSEIF sqlca.SQLCode = 0 THEN
		ll_numero++
END IF

RETURN ll_numero

end function

event open;/*
  istr_mant.argumento[1]	= Planta
  istr_mant.argumento[2]	= Numero Reetiquetado
  istr_mant.argumento[3]	= Cliente
*/

IF gi_CodExport = 300 THEN dw_2.Object.plde_codigo.dddw.Name	=	"dw_mues_plantadesp"

dw_2.GetChild("clie_codigo", dw_clientes)
dw_clientes.SetTransObject(sqlca)
dw_clientes.Retrieve(gi_CodExport)

dw_2.GetChild("plde_codigo", dw_planta)
dw_planta.SetTransObject(sqlca)
dw_planta.Retrieve(-1)

dw_2.GetChild("etiq_codigo", dw_etiqueta)
dw_etiqueta.SetTransObject(sqlca)
dw_etiqueta.Retrieve(-1)

dw_1.GetChild("etiq_codigo", idwc_etiqueta)
idwc_etiqueta.SetTransObject(sqlca)
idwc_etiqueta.Retrieve()

dw_1.GetChild("etiq_numant", idwc_etiquetaant)
idwc_etiquetaant.SetTransObject(sqlca)
idwc_etiquetaant.Retrieve()

call super::open

dw_3.SetTransObject(sqlca)

buscar	= "Pallet Nuevo:Npaen_numero,Variedad:Svari_nombre"
ordenar	= "Pallet Nuevo:paen_numero,Variedad:vari_nombre"

istr_mant.argumento[1]	=	String(gi_CodPlanta)
istr_mant.argumento[3]	=	String(gi_CodExport)
istr_mant.argumento[5]	=	String(Today())

end event

event ue_borra_detalle;call super::ue_borra_detalle;IF dw_1.rowcount() < 2 THEN
	dw_1.SetFocus()
	RETURN
END IF

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

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
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF

	IF dw_1.RowCount() = 0 THEN 
		HabilitaEncab(True)
		pb_eli_det.Enabled = False
	END IF
END IF

istr_mant.borra	 = False
end event

event ue_nuevo_detalle;istr_mant.borra	= False
istr_mant.agrega	= True

istr_mant.Argumento[7]	=	"0"
istr_mant.Argumento[16]	=	"0"
istr_mant.Argumento[21]	=	String(dw_2.Object.plde_codigo[1])

OpenWithParm(iw_mantencion, istr_mant)

IF dw_1.RowCount() > 0 THEN HabilitaEncab(False)

	IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
		pb_eliminar.Enabled	= TRUE
		pb_grabar.Enabled		= TRUE
	END IF

dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila_d = 0, ll_fila_e, respuesta

DO
	dw_1.SetRedraw(False)
	dw_1.Reset()
	ll_fila_e	= dw_1.Retrieve(Integer(istr_mant.argumento[3]),Integer(istr_mant.argumento[1]),Long(istr_mant.argumento[2]))

	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		IF respuesta = 2 THEN Close(This)
	END IF
	dw_1.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)

//dw_1.Enabled	=	False

end event

on w_maed_reetienc_por_despacho.create
int iCurrent
call super::create
this.dw_3=create dw_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_3
end on

on w_maed_reetienc_por_despacho.destroy
call super::destroy
destroy(this.dw_3)
end on

event ue_nuevo;call super::ue_nuevo;HabilitaEncab(True)

dw_2.SetItem(1, "clie_codigo", gi_CodExport)
dw_2.SetItem(1, "plde_codigo", gi_CodPlanta)

istr_mant.argumento[3]	=	String(gi_CodExport)
istr_mant.argumento[1]	=	String(gi_CodPlanta)



end event

event ue_seleccion;call super::ue_seleccion;Long		ll_null

SetNull(ll_null)

istr_busq.argum[1]	=	String(dw_2.Object.clie_codigo[1])
istr_busq.argum[2]	=	String(dw_2.Object.plde_codigo[1])

OpenWithParm(w_busc_reetienc, istr_busq)

istr_busq	=	Message.PowerObjectParm

IF istr_busq.argum[5] <> "" THEN
	istr_mant.argumento[1]	= istr_busq.argum[2]
	istr_mant.argumento[2]	= istr_busq.argum[5]
	istr_mant.argumento[3]  = istr_busq.argum[1]
	IF	istr_mant.argumento[2]	<>	""	THEN
		dw_2.SetItem(1, "reet_numero", Long(istr_mant.argumento[2]))
		IF ExisteReEtiquetado('reet_numero', String(dw_2.Object.reet_numero[1])) THEN
			dw_2.SetFocus()
			dw_2.SetColumn("reet_numero")
			dw_2.SetItem(1, "reet_numero", ll_null)
			RETURN
		END IF
	ELSE
		This.TriggerEvent("ue_recuperadatos")
	END IF
ELSE
	pb_buscar.SetFocus()
END IF

end event

event ue_modifica_detalle;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega			= False
	istr_mant.borra			= False
	istr_mant.Argumento[6]	=	String(dw_1.Object.paen_numero[il_fila])

	OpenWithParm(w_maed_palletencab_consulta, istr_mant)
END IF
end event

event ue_antesguardar;call super::ue_antesguardar;Long	ll_fila_d, ll_fila

IF dw_1.Rowcount() > 0 THEN
	
	IF isnull(dw_2.Object.reet_numero[1]) OR dw_2.Object.reet_numero[1] = 0 THEN
		istr_mant.argumento[15] = String(buscareetiqueta(Integer(istr_mant.argumento[1])))
	END IF	
	
	
	IF Long(Istr_mant.argumento[15]) = 0 THEN
		MessageBox("Advertencia","No Quedan Correlativos Disponibles, Proceda por Mantención.")
		Message.DoubleParm = -1
		Return 
	ELSE
		dw_2.Object.reet_numero[1] = Long(istr_mant.argumento[15])

		FOR ll_fila_d = 1 TO dw_1.RowCount()
			
			ll_fila	=	dw_3.InsertRow(0)
			dw_3.Object.reet_numero[ll_fila] = long(istr_mant.argumento[15])
			dw_3.Object.clie_codigo[ll_fila] = integer(istr_mant.argumento[3])
			dw_3.Object.plde_codigo[ll_fila] = integer(istr_mant.argumento[1])
			dw_3.Object.reet_fereet[ll_fila] = date(istr_mant.argumento[5])
			dw_3.Object.paen_numero[ll_fila] = long(dw_1.Object.paen_numero[ll_fila_d])
			dw_3.Object.etiq_numant[ll_fila] = integer(dw_1.Object.etiq_codigo[ll_fila_d])
			dw_3.Object.etiq_codigo[ll_fila] = Integer(istr_mant.argumento[10])
		NEXT		
		
	END IF	
	

END IF


end event

event ue_listo;//
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_reetienc_por_despacho
integer x = 41
integer y = 840
integer width = 2478
integer height = 1072
integer taborder = 110
string title = "Pallets que Cambiarán Etiqueta"
string dataobject = "dw_info_despafrigode_palletencab"
boolean hsplitscroll = true
boolean righttoleft = true
end type

event dw_1::retrieveend;//If RowCount() > 0 Then	pb_ins_det.Enabled = TRUE
end event

event dw_1::doubleclicked;//
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_reetienc_por_despacho
integer x = 41
integer y = 68
integer width = 2409
integer height = 676
string dataobject = "dw_mant_reetienc_por_despacho"
end type

event dw_2::itemchanged;call super::itemchanged;Long		ll_null
String	ls_columna
Date		ld_nula

SetNull(ll_null)
SetNull(ld_nula)

ls_columna = dwo.Name

CHOOSE CASE ls_columna

	CASE "clie_codigo"
		IF NoExisteCliente(Integer(data)) THEN
			This.SetItem(Row, ls_Columna, gi_codexport)
			RETURN 1
		ELSE
			istr_mant.argumento[3]=data
         dw_2.GetChild("plde_codigo", dw_planta)
			dw_planta.SetTransObject(sqlca)
			dw_planta.Retrieve(1)

			dw_2.GetChild("etiq_codigo", dw_etiqueta)
			dw_etiqueta.SetTransObject(sqlca)
			dw_etiqueta.Retrieve()	
			
			dw_1.GetChild("etiq_codigo", idwc_etiqueta)
			idwc_etiqueta.SetTransObject(sqlca)
			idwc_etiqueta.Retrieve()
			
			dw_1.GetChild("etiq_numant", idwc_etiquetaant)
			idwc_etiquetaant.SetTransObject(sqlca)
			idwc_etiquetaant.Retrieve()			
			IF ExisteGuia(ls_columna, data) THEN
				This.SetItem(Row, ls_Columna, ll_null)
				RETURN 1
			END IF

		END IF

	CASE "plde_codigo"
		IF ExisteReEtiquetado(ls_columna, data) THEN
			This.SetItem(Row, ls_Columna, ll_null)
			RETURN 1
		END IF
		IF ExisteGuia(ls_columna, data) THEN
			This.SetItem(Row, ls_Columna, ll_null)
			RETURN 1
		END IF		
		istr_mant.argumento[1]=data

	CASE "reet_numero"
		IF ExisteReEtiquetado(ls_columna, data) THEN
			This.SetItem(Row, ls_Columna, ll_null)
			RETURN 1
		END IF
		
		istr_mant.argumento[15] = data

	CASE "reet_fereet"
		IF Not f_validafechatempo(date(data)) THEN
			This.SetItem(Row, ls_Columna, ld_nula)
			RETURN 1
		END IF		
		istr_mant.argumento[5] = data
		
	CASE "defe_numero"
		IF ExisteGuia(ls_columna, data) THEN
			This.SetItem(Row, ls_Columna, ll_null)
			RETURN 1
		END IF
	CASE "etiq_codigo"
		IF ExisteEtiqueta(ls_columna, data) THEN
			This.SetItem(Row, ls_Columna, ll_null)
			RETURN 1
		END IF
		
		//reemplazaetiqueta(Integer(data))
			
			

END CHOOSE

HabilitaIngreso(ls_columna)
end event

event dw_2::doubleclicked;//
end event

event dw_2::clicked;call super::clicked;//
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_reetienc_por_despacho
integer x = 2647
integer y = 284
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_reetienc_por_despacho
boolean visible = false
integer x = 2647
integer y = 512
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_reetienc_por_despacho
integer x = 2647
integer y = 728
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_reetienc_por_despacho
boolean visible = false
integer x = 2647
integer y = 960
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_reetienc_por_despacho
integer x = 2647
integer y = 1188
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_reetienc_por_despacho
boolean visible = false
integer x = 2647
integer y = 1532
integer taborder = 90
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_reetienc_por_despacho
boolean visible = false
integer x = 2647
integer y = 1708
integer taborder = 100
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_reetienc_por_despacho
boolean visible = false
integer x = 2647
integer y = 104
boolean enabled = false
end type

type dw_3 from datawindow within w_maed_reetienc_por_despacho
boolean visible = false
integer x = 23
integer y = 1588
integer width = 2528
integer height = 344
integer taborder = 80
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_reetidet_por_despacho"
boolean hscrollbar = true
boolean vscrollbar = true
boolean hsplitscroll = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
boolean righttoleft = true
end type

