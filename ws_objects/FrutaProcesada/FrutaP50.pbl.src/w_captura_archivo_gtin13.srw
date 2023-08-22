$PBExportHeader$w_captura_archivo_gtin13.srw
$PBExportComments$Ventana que captura archivo plano de cajas
forward
global type w_captura_archivo_gtin13 from w_mant_encab_deta
end type
type pb_archivo from picturebutton within w_captura_archivo_gtin13
end type
end forward

global type w_captura_archivo_gtin13 from w_mant_encab_deta
integer width = 3621
integer height = 2084
string title = "Captura Archivo de GS1 GTIN 13"
string menuname = ""
event ue_validaregistro ( )
event ue_carga_detalle ( )
pb_archivo pb_archivo
end type
global w_captura_archivo_gtin13 w_captura_archivo_gtin13

type variables
String 		is_Archivo, is_mensaje, is_Docname, is_dato
Integer		ii_valida

OleObject	myoleobject






end variables

forward prototypes
public subroutine habilitaencab (boolean habilita)
public subroutine habilitaingreso ()
public function integer buscavarrelacionada (integer ai_especie, integer ai_variedad)
public function string buscaprodrotulado (integer ai_cliente, long al_productor)
protected function boolean wf_actualiza_db (boolean borrando)
public function long buscanumero (integer ai_planta)
public function long buscanuevofolio (integer cliente, integer planta)
public function boolean existecaja (integer ai_cliente, integer ai_planta, long al_caja, boolean ab_mensaje)
public function boolean existepallet (integer ai_cliente, integer ai_planta, long al_pallet, boolean ab_mensaje)
public function boolean existepalletendestino (integer ai_cliente, integer ai_planta, long al_pallet)
public subroutine carga_datos (integer al_fila, integer al_detalle)
end prototypes

event ue_carga_detalle();Integer	li_Coneccion, li_Encab, li_Condensacion, li_Contador, li_zona, li_cont
Integer  li_productor, li_valida=0, li_especie, li_variedad, li_familia, li_servicio, li_tipoenva, li_envase, li_pool
Long		ll_Fila, ll_Detalle=0, tt, t
String   ls_nula, ls_colu[], ls_mensaje, ls_embalaje, li_codmat
Decimal{4} li_valor

ii_valida=0
SetPointer(HourGlass!)
myoleobject			= CREATE OLEObject 
w_captura_archivo_gtin13.TriggerEvent("ue_borrar")
//dw_1.Reset()
li_Coneccion = myoleobject.ConnectToObject(is_Docname) 
w_main.SetMicroHelp("Cargando Información...")
IF li_Coneccion = 0 THEN 
	ll_Fila = 13
   li_Contador = 0
   DO WHILE li_Contador < 1
		ll_Fila ++
		is_dato  = Trim(String(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,1).value))
		IF	(is_dato = "" OR  IsNull(is_dato))  THEN
		   li_Contador ++
		ELSE
			carga_datos(ll_fila,ll_detalle)
		END IF
	LOOP

	myoleobject.disconnectobject()
	Destroy myoleobject
	
	dw_1.SetSort('#3 A , #4 A, #5 A') 
	dw_1.Sort()
	 
	MessageBox('Atención','Proceso Terminado, Debe Grabar',Exclamation!)
	pb_grabar.Enabled = TRUE

ELSE
	MessageBox('Atención','Error de Conexión Con Planilla Excel',Exclamation!)
END IF 

end event

public subroutine habilitaencab (boolean habilita);//IF Habilita THEN
//	dw_2.Enabled	= True
//	dw_2.Object.clpr_rut.Protect=1
//	dw_2.Object.clpr_nombre.Protect=1
//	dw_2.Object.mden_docrel.Protect=1
//	dw_2.Object.ocen_numero.Protect=1
//	dw_2.Object.reen_chofer.Protect=1
//	dw_2.Object.reen_patent.Protect=1
//	dw_2.Object.reen_dirdes.Protect=1
//	dw_2.Object.mden_fecdre.Protect=1
//	dw_2.SetTabOrder("bode_codigo", 10)
//	dw_2.Modify("bode_codigo.BackGround.Color = " + String(rgb(255,255,255)))
//END IF
//
//RETURN
end subroutine

public subroutine habilitaingreso ();//IF istr_mant.solo_consulta THEN RETURN
//
//Boolean	Habilita	= True
//
//dw_2.AcceptText()
//
//IF Isnull(dw_2.GetItemString(1,"clpr_rut")) THEN
//	Habilita = False
//ELSEIF Isnull(dw_2.GetItemNumber(1,"ocen_numero")) THEN
//	Habilita = False
//ELSEIF Isnull(dw_2.GetItemNumber(1,"mden_docrel")) THEN
//	Habilita = False
//ELSEIF Isnull(dw_2.GetItemDate(1,"mden_fecdre")) THEN
//	Habilita = False
//
//END IF
//
//IF is_nuevo = 'N' THEN
//	Habilita = False
//END IF
//
//pb_ins_det.Enabled	= Habilita
end subroutine

public function integer buscavarrelacionada (integer ai_especie, integer ai_variedad);Integer li_variedad

  SELECT vari_relaci  
    INTO :li_variedad  
    FROM dba.variedades  
   WHERE espe_codigo = :ai_especie  AND  
         vari_codigo = :ai_variedad;

IF sqlca.SQLCode =-1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla variedades")
	
END IF

Return li_variedad




end function

public function string buscaprodrotulado (integer ai_cliente, long al_productor);String ls_productor

  SELECT prpr_codigo  
    INTO :ls_productor  
    FROM dba.productoresprod  
   WHERE clie_codigo = :ai_cliente  AND  
         prod_codigo = :al_productor;

IF sqlca.SQLCode =-1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla productoresprod")
	
END IF

Return ls_productor




end function

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

Borrando	=	False

IF Borrando THEN

//	ELSE
//		F_ErrorBaseDatos(sqlca, This.Title)
//		
//		RollBack;
//	END IF		
ELSE
	IF dw_1.Update(True, False) = 1 THEN
		
		Commit;
		
		IF sqlca.SQLCode <> 0 THEN
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		ELSE
			lb_Retorno	=	True
			
			dw_1.ResetUpdate()
		
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
		
		RollBack;
	END IF	
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public function long buscanumero (integer ai_planta);Long ll_numero

  SELECT Max(rfpe_numero)  
    INTO :ll_numero  
    FROM dba.recfruprocee  
   WHERE plde_codigo = :ai_planta;


IF sqlca.SQLCode =-1 THEN
	F_ErrorBaseDatos(sqlca,"Problema para obtener número Recepcion")
END IF

IF Isnull(ll_numero) OR ll_numero = 0 THEN
	MessageBox("Atención","No existe número de correlativo en el mantenedor.",&
							Exclamation!, OK!)
ELSE
	ll_numero = ll_numero + 1 
END IF	

Return ll_numero




end function

public function long buscanuevofolio (integer cliente, integer planta);Integer	li_planta, li_tipoins, li_movto
Long		ll_numero,ll_numero2, ll_fin, ll_actual

li_planta	=	planta

li_movto = 1

SELECT Max(rfpe_numero)
  INTO :ll_numero
  FROM DBA.RECFRUPROCEE
 WHERE plde_codigo = :li_planta;
 
Select como_inicia, como_actual, como_termin
Into	:ll_numero2, :ll_actual, :ll_fin
from DBA.CORRELMOVIMIENTOS 
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
	F_errorbasedatos(sqlca,"Lectura tabla RecFruProcee")
ELSEIF sqlca.SQLCode = 0 THEN
	ll_numero++
END IF

RETURN ll_numero









end function

public function boolean existecaja (integer ai_cliente, integer ai_planta, long al_caja, boolean ab_mensaje);Long ll_numero

SELECT capr_numero  
  INTO :ll_numero  
  FROM dba.spro_cajasprod_trans 
  WHERE clie_codigo = :ai_cliente AND  
        plde_codigo = :ai_planta  AND  
        capr_numero = :al_caja;

IF sqlca.SQLCode =-1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Spro_cajasprod_trans")
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	RETURN FALSE
ELSEIf ab_mensaje THEN
	MessageBox("Atención","Archivo de Cajas ya fue Ingresado.",&
							Exclamation!, OK!)
	RETURN TRUE
ELSE
	RETURN TRUE
END IF




end function

public function boolean existepallet (integer ai_cliente, integer ai_planta, long al_pallet, boolean ab_mensaje);Long ll_numero

SELECT paen_numero  
  INTO :ll_numero  
  FROM dba.Palletencab_trans 
  WHERE clie_codigo = :ai_cliente AND  
        plde_codigo = :ai_planta  AND  
        paen_numero = :al_pallet;

IF sqlca.SQLCode =-1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Spro_cajasprod_trans")
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	RETURN FALSE
ELSEIf ab_mensaje AND IsNull(ll_numero) AND ll_numero=0 THEN
	MessageBox("Atención","Archivo de Cajas ya fue Ingresado.",&
							Exclamation!, OK!)
	RETURN TRUE
ELSE
	RETURN TRUE
END IF




end function

public function boolean existepalletendestino (integer ai_cliente, integer ai_planta, long al_pallet);Long ll_numero

SELECT paen_numero  
  INTO :ll_numero  
  FROM dba.Palletencab
  WHERE clie_codigo = :ai_cliente AND  
        plde_codigo = :ai_planta  AND  
        paen_numero = :al_pallet;

IF sqlca.SQLCode =-1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Palletencab")
	RETURN False
ELSEIF sqlca.SQLCode = 100 OR IsNull(ll_numero) OR ll_numero=0 THEN

		SELECT paen_numero  
	   INTO :ll_numero  
  		FROM dba.Palletencab_trans
  		WHERE clie_codigo = :ai_cliente AND  
        		plde_codigo = :ai_planta  AND  
        		paen_numero = :al_pallet;
	
		IF sqlca.SQLCode =-1 THEN
			F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Palletencab_Trans")
			RETURN False
		ELSEIF sqlca.SQLCode = 100 OR IsNull(ll_numero) OR ll_numero=0 THEN

			RETURN TRUE
		ELSE
			RETURN False
		END IF
ELSE
	RETURN False
END IF


end function

public subroutine carga_datos (integer al_fila, integer al_detalle);Decimal{4} li_valor, ld_valor
Long tt
Integer li_zona
String  li_codmat, ls_valor

//FOR tt=1 TO 45
	 al_detalle = dw_1.InsertRow(0)
	 ls_valor   = String(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,1).value)
	 dw_1.Setitem(al_detalle ,"GTIN_NUMERO",dec(ls_valor))
	// IF tt=1 THEN
		 ls_valor   = String(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,2).value)
	    dw_1.Setitem(al_detalle ,"GTIN_TIPCOD",(ls_valor))
	//ELSEIF tt=2 THEN
		 ls_valor   = string(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,3).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_TIPPRD",ls_valor)
	//ELSEIF tt=3 THEN
		 ls_valor   = String(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,4).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_UNIDAD",ls_valor)
	//ELSEIF tt=4 THEN
		 ls_valor   = string(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,5).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_PROCED",ls_valor)
	//ELSEIF tt=5 THEN
		 ls_valor   = String(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,6).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_DESCRI",ls_valor)
	//ELSEIF tt=6 THEN
	    ls_valor   = String(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,7).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_MARCA",ls_valor)
	//ELSEIF tt=7 THEN
		 ls_valor   = String(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,8).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_CONTEN",Long(ls_valor))
	//ELSEIF tt=8 THEN
		 ls_valor   = string(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,9).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_UNIMED",ls_valor)
	//ELSEIF tt=9 THEN
		 ls_valor   = String(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,10).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_ENVASE",ls_valor)
	//ELSEIF tt=10 THEN
		 ls_valor   = string(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,11).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_CODEMP",Long(ls_valor))
	//ELSEIF tt=11 THEN
		 ls_valor   = String(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,12).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_EMPRES",ls_valor)
	//ELSEIF tt=12 THEN
	    ls_valor   = String(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,13).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_UNICON",Long(ls_valor))
	//ELSEIF tt=13 THEN
		 ls_valor   = String(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,14).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_CANCON",Long(ls_valor))
	//ELSEIF tt=14 THEN
		 ls_valor   = string(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,15).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_CODECR",Long(ls_valor))
	//ELSEIF tt=15 THEN
		 ls_valor   = String(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,16).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_CATECR",ls_valor)
	//ELSEIF tt=16 THEN
		 ls_valor   = string(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,17).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_CCATUN",Long(ls_valor))
	//ELSEIF tt=17 THEN
		 ls_valor   = String(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,18).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_CATUNI",ls_valor)
	//ELSEIF tt=18 THEN
	    ls_valor   = String(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,19).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_CODINT",ls_valor)
	//ELSEIF tt=19 THEN
		 ls_valor   = String(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,20).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_REGISP",ls_valor)	 
	//ELSEIF tt=20 THEN
		 ls_valor   = string(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,21).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_MODELO",ls_valor)
	//ELSEIF tt=21 THEN
		 ls_valor   = String(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,22).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_DURACI",ls_valor)
	//ELSEIF tt=22 THEN
		 ls_valor   = string(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,23).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_ALTO",dec(ls_valor))
	//ELSEIF tt=23 THEN
		 ls_valor   = String(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,24).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_ANCHO",dec(ls_valor))
	//ELSEIF tt=24 THEN
	    ls_valor   = String(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,25).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_FONDO",dec(ls_valor))
	//ELSEIF tt=25 THEN
		 ls_valor   = String(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,26).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_PBRUTO",dec(ls_valor))	
	//ELSEIF tt=26 THEN
		 ls_valor   = string(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,27).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_PDRENA",dec(ls_valor))
	//ELSEIF tt=27 THEN
		 ls_valor   = String(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,28).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_MAXAPI",ls_valor)
	//ELSEIF tt=28 THEN
		 ls_valor   = string(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,29).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_TALLA",ls_valor)
	//ELSEIF tt=29 THEN
		 ls_valor   = String(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,30).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_COLOR",ls_valor)
	//ELSEIF tt=30 THEN
	    ls_valor   = String(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,31).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_CAMADA",dec(ls_valor))
	//ELSEIF tt=31 THEN
		 ls_valor   = String(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,32).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_CUNCAM",dec(ls_valor))	
	//ELSEIF tt=32 THEN
		 ls_valor   = string(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,33).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_CARACT",ls_valor)
	//ELSEIF tt=33 THEN
		 ls_valor   = String(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,35).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_PERFUM",ls_valor)
	//ELSEIF tt=34 THEN
		 ls_valor   = string(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,34).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_INFLAM",ls_valor)
	//ELSEIF tt=35 THEN
		 ls_valor   = string(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,36).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_TEMMIN",dec(ls_valor))
	//ELSEIF tt=36 THEN
	    ls_valor   = string(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,37).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_TEMMAX",dec(ls_valor))
	//ELSEIF tt=37 THEN
		 ls_valor   = String(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,38).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_FLEJE",(ls_valor))
	//ELSEIF tt=38 THEN
		 ls_valor   = string(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,39).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_DISSEG",ls_valor)
	//ELSEIF tt=39 THEN
		 ls_valor   = String(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,40).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_FOTO",ls_valor)
	//ELSEIF tt=40 THEN
		 ls_valor   = string(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,41).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_INFLEC",ls_valor)
	//ELSEIF tt=41 THEN
		 ls_valor   = String(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,42).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_CONFID",ls_valor)
	//ELSEIF tt=42 THEN
	    ls_valor   = String(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,43).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_FECING",ls_valor)
	//ELSEIF tt=43 THEN
		 ls_valor   = String(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,44).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_FECACT",ls_valor)	 
	//ELSEIF tt=44 THEN
		 ls_valor   = string(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,45).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_FECVER",ls_valor)
	//ELSEIF tt=45 THEN
		 ls_valor   = String(myoleobject.application.workbooks(1).worksheets(1).cells(al_Fila,46).value) 
		 dw_1.Setitem(al_detalle ,"GTIN_FECELI",ls_valor)
	//END IF	 
//NEXT
end subroutine

on w_captura_archivo_gtin13.create
int iCurrent
call super::create
this.pb_archivo=create pb_archivo
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.pb_archivo
end on

on w_captura_archivo_gtin13.destroy
call super::destroy
destroy(this.pb_archivo)
end on

event open;call super::open;im_menu	= m_principal

This.ParentWindow().ToolBarVisible	= True
im_menu.Item[1].Item[6].Enabled		= True
im_menu.Item[7].Enabled					= True

istr_Mant.Argumento[1]	=	String(gi_CodPlanta)	 

pb_buscar.PostEvent(Clicked!)










end event

event ue_nuevo;call super::ue_nuevo;Long		ll_modif1, ll_modif2

ib_ok	= True

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta

IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			ib_ok = False
		CASE 0
			ll_modif1	=	dw_1.GetNextModified(0, Primary!)
			ll_modif2	=	dw_2.GetNextModified(0, Primary!)
		
//			IF ib_ModEncab OR dw_1.GetNextModified(0, Primary!) > 0 THEN
			IF dw_1.RowCount() > 0 THEN
				CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
					CASE 1
						Message.DoubleParm = 0
						This.TriggerEvent("ue_guardar")
						IF message.DoubleParm = -1 THEN ib_ok = False
					CASE 3
						ib_ok	= False
						RETURN
				END CHOOSE
			END IF
	END CHOOSE
END IF

IF Not ib_ok THEN RETURN

dw_1.Reset()

pb_eli_det.Enabled		=	False
pb_ins_det.Enabled		=	False
pb_grabar.Enabled			=	False
pb_eliminar.Enabled		=	False
pb_imprimir.Enabled		=	False

pb_archivo.Enabled = TRUE




end event

event ue_imprimir;SetPointer(HourGlass!)
Integer	li_Cliente, li_Planta
Long		fila, ll_Pallet
str_info	lstr_info

IF dw_1.RowCount() > 0 THEN
	li_Cliente	= dw_1.Object.clie_codigo[1]
	li_Planta	= dw_1.Object.plde_codigo[1]
	ll_Pallet	= dw_1.Object.capr_numpal[1]
	 
	lstr_info.titulo	= "RECEPCIÓN ARCHIVO DE CAJAS"
	lstr_info.copias	= 1
	
	OpenWithParm(vinf,lstr_info)
	
	vinf.dw_1.DataObject = "dw_info_captura_archivocajas" 	
	vinf.dw_1.SetTransObject(sqlca)
	
	dw_1.ShareData(vinf.dw_1)
	Fila	=	dw_1.RowCount()
	//fila = vinf.dw_1.Retrieve(li_Cliente,li_Planta,ll_Pallet)
	
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
END IF
end event

event resize;//
Integer	maximo, li_posic_x, li_posic_y, li_visible = 0

IF dw_2.width > il_AnchoDw_1 THEN
	maximo		=	dw_2.width
ELSE
	dw_1.width	=	This.WorkSpaceWidth() - 400
//	st_1.width	=	This.WorkSpaceWidth() - 400
	maximo		=	dw_1.width
END IF
dw_1.height				= This.WorkSpaceHeight() - dw_1.y - 41


li_posic_x				= This.WorkSpaceWidth() - 240
li_posic_y				= 300

IF pb_buscar.Visible THEN
	pb_buscar.x				= li_posic_x
	pb_buscar.y				= li_posic_y
	pb_buscar.width		= 233
	pb_buscar.height		= 196
	li_visible ++
	li_posic_y += 195
END IF

IF pb_nuevo.Visible THEN
	pb_nuevo.x				= li_posic_x
	pb_nuevo.y				= li_posic_y
	pb_nuevo.width			= 233
	pb_nuevo.height		= 196
	li_visible ++
	li_posic_y += 195
END IF

IF pb_grabar.Visible THEN
	pb_grabar.x				= li_posic_x
	pb_grabar.y				= li_posic_y
	pb_grabar.width		= 233
	pb_grabar.height		= 196
	li_visible ++
	li_posic_y += 195
END IF

IF pb_imprimir.Visible THEN
	pb_imprimir.x			= li_posic_x
	pb_imprimir.y			= li_posic_y
	pb_imprimir.width		= 233
	pb_imprimir.height	= 196
	li_visible ++
	li_posic_y += 195
END IF


IF pb_archivo.Visible THEN
	pb_archivo.x			= li_posic_x
	pb_archivo.y			= li_posic_y
	pb_archivo.width		= 233
	pb_archivo.height		= 196
	li_visible ++
	li_posic_y += 195
END IF

IF	pb_eliminar.Visible THEN
	pb_eliminar.x			= li_posic_x
	pb_eliminar.y			= li_posic_y
	pb_eliminar.width		= 233
	pb_eliminar.height	= 196
	li_visible ++
	li_posic_y += 195
END IF

IF pb_salir.Visible THEN
	pb_salir.x				= li_posic_x
	pb_salir.y				= li_posic_y
	pb_salir.width			= 233
	pb_salir.height		= 196
	li_visible ++
	li_posic_y += 195
END IF

pb_ins_det.x			= li_posic_x
pb_ins_det.y			= 1230
pb_ins_det.width		= 233
pb_ins_det.height		= 196

pb_eli_det.x			= li_posic_x
pb_eli_det.y			= pb_ins_det.y + 195
pb_eli_det.width		= 233
pb_eli_det.height		= 196


end event

event ue_guardar;Integer	li_Agrupa


IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")
Message.DoubleParm = 0
TriggerEvent("ue_antesguardar")


IF wf_actualiza_db(False) THEN
	pb_imprimir.Enabled = TRUE
	w_main.SetMicroHelp("Información Grabada.")
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	
	RETURN
END IF

end event

event ue_listo;IF dw_1.RowCount() > 0 THEN
	IF istr_mant.Solo_Consulta THEN
		dw_2.Enabled			=	False
   	pb_Grabar.Enabled		=	False
		//pb_ins_det.Enabled	=	False
		//pb_eli_det.Enabled	=	False
	ELSE
		dw_2.Enabled			=	True
		pb_Eliminar.Enabled	=	True
		pb_Grabar.Enabled		=	True
		//pb_ins_det.Enabled	=	True
		//pb_eli_det.Enabled	=	True
	END IF
//ELSE
//	IF istr_mant.Solo_Consulta THEN
//		pb_ins_det.Enabled	=	False
//	ELSE
//		pb_ins_det.Enabled	=	True
//	END IF
END IF

w_main.SetMicroHelp("Listo")

SetPointer(Arrow!)
end event

event ue_recuperadatos;call super::ue_recuperadatos;

dw_1.Retrieve()

end event

event ue_borrar;IF dw_1.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF dw_1.RowCount() > 0 THEN dw_1.RowsMove(1,dw_1.RowCount(),Primary!,dw_1,1,Delete!)
w_main.SetMicroHelp("Borrando Registro...")
IF wf_actualiza_db(True) THEN
	w_main.SetMicroHelp("Registro Borrado...")
	SetPointer(Arrow!)
ELSE
	w_main.SetMicroHelp("Registro no Borrado...")
END IF			

end event

event ue_validaborrar;//
end event

type dw_1 from w_mant_encab_deta`dw_1 within w_captura_archivo_gtin13
integer x = 82
integer y = 28
integer width = 2551
integer height = 1780
integer taborder = 100
string title = "Detalle de Captura"
string dataobject = "dw_mues_excel_gtin13"
end type

type dw_2 from w_mant_encab_deta`dw_2 within w_captura_archivo_gtin13
boolean visible = false
integer x = 704
integer y = 1188
integer width = 229
integer height = 92
end type

event dw_2::doubleclicked;//
end event

event dw_2::itemfocuschanged;call super::itemfocuschanged;//IF is_rut <> "" THEN
//	IF Len(is_rut) < 10 THEN
//		This.Object.clpr_rut.Format = '@@@@@@'
//	ELSE
//		This.Object.clpr_rut.Format = '@@@.@@@.@@@-@'
//	END IF
//	
//	IF dwo.name <> "clpr_rut" THEN
//		This.SetItem(1, "clpr_rut", is_rut)
//	END IF
//END IF
end event

event dw_2::help;call super::help;//Boolean	lb_AutoCommit, lb_Retorno
//Integer	li_concon
//
//IF Not dw_2.uf_check_required(0) THEN RETURN False
//
//IF Not dw_1.uf_validate(0) THEN RETURN False
//
//lb_AutoCommit		=	sqlca.AutoCommit
//sqlca.AutoCommit	=	False
//
//IF Borrando THEN
//	IF dw_1.Update() = -1 THEN
//		F_ErrorBaseDatos(sqlca, This.Title)
//	 ELSEIF dw_2.Update() = -1 THEN
//		       F_ErrorBaseDatos(sqlca, This.Title)
//			    RollBack;
//			 ELSE
//				// Bloque Grabación
//				SELECT Expa_concon INTO :li_concon
//				FROM dba.Existeparam
//				WHERE expa_identi = 1;
//				UPDATE dba.Existeparam SET
//				expa_concon = li_concon
//				WHERE expa_identi = 1;
//				//
//				
//		       Commit;
//		
//		       IF sqlca.SQLCode <> 0 THEN
//			       F_ErrorBaseDatos(sqlca, This.Title)
//		       ELSE
//			       lb_Retorno	=	True
//		       END IF
//	       END IF
//ELSE
//	IF dw_2.Update() = -1 THEN
//		F_ErrorBaseDatos(sqlca, This.Title)
//	ELSEIF dw_1.Update() = -1 THEN
//		     F_ErrorBaseDatos(sqlca, This.Title)
//		 RollBack;
//	ELSE
//		// Bloque Grabación
//		SELECT Expa_concon INTO :li_concon
//		FROM dba.Existeparam
//		WHERE expa_identi = 1;
//		UPDATE dba.Existeparam SET
//		expa_concon = li_concon
//		WHERE expa_identi = 1;
//		//		
//		Commit;
//		
//		IF sqlca.SQLCode <> 0 THEN
//			F_ErrorBaseDatos(sqlca, This.Title)
//		ELSE
//			lb_Retorno	=	True
//		END IF
//	END IF
//END IF
//
//sqlca.AutoCommit	=	lb_AutoCommit
//
//RETURN lb_Retorno
end event

event dw_2::itemchanged;call super::itemchanged;//String       ls_columna
//Integer      li_null
//
//SetNull(li_null)
//
//ls_columna = dwo.name
//
//CHOOSE CASE ls_columna
//	CASE "bode_codigo"
//		IF NoExisteBodega(Integer(data)) THEN
//		   This.SetItem(il_fila, "bode_codigo", li_null)
//		   RETURN 1
//		ELSE
//			pb_correo.Enabled = TRUE
//	   END IF	
//		
//END CHOOSE		
end event

type pb_nuevo from w_mant_encab_deta`pb_nuevo within w_captura_archivo_gtin13
boolean visible = false
integer x = 3264
integer y = 380
boolean enabled = false
alignment htextalign = right!
end type

type pb_eliminar from w_mant_encab_deta`pb_eliminar within w_captura_archivo_gtin13
boolean visible = false
integer x = 3264
integer y = 1100
string picturename = "\desarrollo\bmp\anulae.bmp"
string disabledname = "\desarrollo\bmp\anulad.bmp"
end type

event pb_eliminar::clicked;//Boolean		lb_mespro
//Date			ld_fecha
//Integer		li_tipmov
//Long			ll_numero, ll_ocurre
//
//
//is_emitir	=  ''
//ld_fecha		=	Date('01/'+Mid(istr_mant.argumento[3],4))
//
//IF  ld_fecha < gstr_param.mes_proceso THEN
//				istr_mant.solo_consulta = True
//				MessageBox("Error de Inconsistencia", "No Puede Eliminar Movimiento con Fecha Anterior al Mes de Proceso.")
//				lb_mespro = True
//ELSE
//				istr_mant.solo_consulta = False
//				lb_mespro = False
//END IF
//
//
//li_tipmov	=	Integer(istr_mant.argumento[1])
//ll_numero	=	Long(istr_mant.argumento[2])
//
//IF li_tipmov =  1 THEN
//	
//			DECLARE revisa_saldos PROCEDURE FOR dba.Exis_RevisaSaldosItemsdeGuia  
//						@tipdoc = :li_tipmov,   
//						@numero = :ll_numero  ;
//			EXECUTE revisa_saldos ;
//			
//			IF SQLCA.SQLCode < 0 THEN
//				MessageBox("Error en Revisión de Saldos", "Se ha producido un Error en Revisión de Saldos.~r~r" + &
//								SQLCA.SQLErrText)
//			ELSE
//			   FETCH revisa_saldos INTO :ll_ocurre ;	// Numero de Documento Generado
//			   CLOSE revisa_saldos;
//			
//				IF ll_ocurre > 0 THEN
//					MessageBox("Saldo Negativo", "No Puede Eliminar Movimiento: Un(os) Item(s) Quedará(n) con Saldo Negativo.")
//				END IF
//				
//			END IF
//ELSE
//		ll_ocurre = 0
//END IF
//	
//
//IF lb_mespro = False AND ll_ocurre =0 THEN
//
//	CALL SUPER::Clicked
//
//END IF
end event

type pb_grabar from w_mant_encab_deta`pb_grabar within w_captura_archivo_gtin13
integer x = 3264
integer y = 556
end type

type pb_imprimir from w_mant_encab_deta`pb_imprimir within w_captura_archivo_gtin13
boolean visible = false
integer x = 3264
integer y = 740
end type

type pb_salir from w_mant_encab_deta`pb_salir within w_captura_archivo_gtin13
integer x = 3264
integer y = 1280
end type

type pb_ins_det from w_mant_encab_deta`pb_ins_det within w_captura_archivo_gtin13
boolean visible = false
integer y = 1528
end type

type pb_eli_det from w_mant_encab_deta`pb_eli_det within w_captura_archivo_gtin13
boolean visible = false
integer y = 1700
end type

type pb_buscar from w_mant_encab_deta`pb_buscar within w_captura_archivo_gtin13
integer x = 3264
integer y = 200
end type

event pb_buscar::clicked;Parent.TriggerEvent("ue_recuperadatos")
end event

type pb_archivo from picturebutton within w_captura_archivo_gtin13
integer x = 3264
integer y = 916
integer width = 233
integer height = 196
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean default = true
string picturename = "F:\Desarrollo 12\Imagenes\Botones\087.png"
alignment htextalign = left!
end type

event clicked;String	ls_directorio, ls_archivo, path, nombre, pathpordefault
Integer	li_valida, li_opcion = 1, li_Valor, li_respuesta
Long		ll_rc
dwitemstatus stat

IF dw_1.RowCount() > 0 THEN

	li_Respuesta = 	MessageBox("Atención Tabla Ya Contiene Datos","Desea Remplazar Registros",Exclamation!, YesNo!, 1) 
	IF li_Respuesta = 1 THEN
		RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, pathpordefault)

		li_Valor = GetFileOpenName("Carga de Archivo",is_Docname, nombre, "XLS", &
			 + "Excel Files (*.xls),*.xls," &
			 + "Excel Files (*.xls),*.xls")
			 
		IF li_Valor < 1 THEN
			MessageBox("Atención","Error al Abrir Archivo",Exclamation!)
			RETURN  
		ELSE
			w_captura_archivo_gtin13.TriggerEvent("ue_carga_detalle")
		END IF
	ELSE
		Return 1
	END IF
ELSE
	RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, pathpordefault)

	li_Valor = GetFileOpenName("Carga de Archivo",is_Docname, nombre, "XLS", &
		 + "Excel Files (*.xls),*.xls," &
		 + "Excel Files (*.xls),*.xls")
		 
	IF li_Valor < 1 THEN
		MessageBox("Atención","Error al Abrir Archivo",Exclamation!)
		RETURN  
	ELSE
		w_captura_archivo_gtin13.TriggerEvent("ue_carga_detalle")
	END IF
END IF	



	

	


	
	




end event

